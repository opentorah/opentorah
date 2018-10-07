package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Named, Names, Metadata, XML, Holder}

import scala.xml.Elem

object Tanach extends Named {

  override type Key = TanachBook

  sealed trait TanachBook extends Named.NamedBase {
    final def chapters: Chapters = toChapters(this)
  }

  override lazy val toNames: Map[TanachBook, Names] =
    metadata.get.mapValues(_.names)

  private lazy val toChapters: Map[TanachBook, Chapters] =
    metadata.get.mapValues(metadata => Chapters(metadata.chapterElements))

  sealed abstract class ChumashBook(val parshiot: Seq[Parsha]) extends TanachBook {
    private val metadata = new Holder[Seq[Metadata]] {
      protected override def load: Seq[Metadata] = Tanach.metadata.get(ChumashBook.this).weekElements
        .map(element => Metadata.loadSubresource(this, element, "week"))
    }

    lazy val weeks: Map[Parsha, Parsha.Structure] = Parsha.parse(this, metadata.get)

    final override def names: Names =  weeks(parshiot.head).names
  }

  case object Genesis extends ChumashBook(Parsha.genesis)
  case object Exodus extends ChumashBook(Parsha.exodus)
  case object Leviticus extends ChumashBook(Parsha.leviticus)
  case object Numbers extends ChumashBook(Parsha.numbers)
  case object Deuteronomy extends ChumashBook(Parsha.deuteronomy)

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  sealed trait NachBook extends TanachBook {
    final override def names: Names = toNames(this)
  }

  sealed trait ProphetsBook extends NachBook

  sealed trait EarlyProphetsBook extends ProphetsBook

  case object Joshua extends EarlyProphetsBook
  case object Judges extends EarlyProphetsBook
  case object SamuelI extends EarlyProphetsBook { override def name: String = "I Samuel" }
  case object SamuelII extends EarlyProphetsBook { override def name: String = "II Samuel" }
  case object KingsI extends EarlyProphetsBook { override def name: String = "I Kings" }
  case object KingsII extends EarlyProphetsBook { override def name: String = "II Kings" }

  val earlyProphets: Seq[ProphetsBook] = Seq(Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII)

  sealed trait LateProphetsBook extends ProphetsBook

  case object Isaiah extends LateProphetsBook
  case object Jeremiah extends LateProphetsBook
  case object Ezekiel extends LateProphetsBook

  <!-- תרי עשר -->
  sealed trait TreiAsarBook extends LateProphetsBook

  case object Hosea extends TreiAsarBook
  case object Joel extends TreiAsarBook
  case object Amos extends TreiAsarBook
  case object Obadiah extends TreiAsarBook
  case object Jonah extends TreiAsarBook
  case object Micah extends TreiAsarBook
  case object Nahum extends TreiAsarBook
  case object Habakkuk extends TreiAsarBook
  case object Zephaniah extends TreiAsarBook
  case object Haggai extends TreiAsarBook
  case object Zechariah extends TreiAsarBook
  case object Malachi extends TreiAsarBook

  val treiAsar: Seq[TreiAsarBook] = Seq(Hosea, Joel, Amos, Obadiah, Jonah, Micah,
    Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi)

  val lateProphets: Seq[ProphetsBook] = Seq(Isaiah, Jeremiah, Ezekiel) ++ treiAsar

  val prophets: Seq[ProphetsBook] = earlyProphets ++ lateProphets

  def getProhetForName(name: String): ProphetsBook = getForName(name).asInstanceOf[ProphetsBook]

  sealed trait WritingsBook extends NachBook

  case object Psalms extends WritingsBook
  case object Proverbs extends WritingsBook
  case object Job extends WritingsBook
  case object SongOfSongs extends WritingsBook { override def name: String = "Song of Songs" }
  case object Ruth extends WritingsBook
  case object Lamentations extends WritingsBook
  case object Ecclesiastes extends WritingsBook
  case object Esther extends WritingsBook
  case object Daniel extends WritingsBook
  case object Ezra extends WritingsBook
  case object Nehemiah extends WritingsBook
  case object ChroniclesI extends WritingsBook { override def name: String = "I Chronicles" }
  case object ChroniclesII extends WritingsBook { override def name: String = "II Chronicles" }

  val writings: Seq[WritingsBook] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)

  val nach: Seq[TanachBook] = prophets ++ writings

  override val values: Seq[TanachBook] = chumash ++ nach

  private final case class TanachMetadata(names: Names, chapterElements: Seq[Elem], weekElements: Seq[Elem])

  private val metadata = new Holder[Map[TanachBook, TanachMetadata]] {
    protected override def load: Map[TanachBook, TanachMetadata] = Metadata.loadMetadata(
      values = values,
      obj = this,
      resourceName = "Tanach",
      rootElementName = "metadata",
      elementName = "book"
    ).map { case (book, bookMetadata) =>
      bookMetadata.attributes.close()
      val (chapterElements: Seq[Elem], weekElements: Seq[Elem]) =
        XML.span(bookMetadata.elements, "chapter", "week")
      if (!book.isInstanceOf[ChumashBook]) XML.checkNoMoreElements(weekElements)
      book -> TanachMetadata(bookMetadata.names, chapterElements, weekElements)
    }
  }
}
