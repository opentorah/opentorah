package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Named, Names, Metadata, XML}

import scala.ref.WeakReference
import scala.xml.Elem

object Tanach extends Named {

  override type Key = Book

  sealed trait Book extends Named.NamedBase {
    override def names: Names = toNames(this)

    final def chapters: Chapters = toChapters(this)
  }

  private lazy val toNames: Map[Book, Names] =
    getToMetadata.mapValues(_.names)

  private lazy val toChapters: Map[Book, Chapters] =
    getToMetadata.mapValues(metadata => Chapters(metadata.chapterElements))

  sealed trait ChumashBook extends Book {
    final def parshiot: Seq[Parsha.Parsha] = Parsha.values.filter(_.book == this)

    private lazy val weeks: Map[Parsha.Parsha, Parsha.Structure] =
      Parsha.parse(this, getToMetadata(this).weekElements)

    final override def names: Names =  weeks(parshiot.head).names
  }

  case object Genesis extends ChumashBook
  case object Exodus extends ChumashBook
  case object Leviticus extends ChumashBook
  case object Numbers extends ChumashBook
  case object Deuteronomy extends ChumashBook

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  trait ProphetsBook extends Book

  trait EarlyProphetsBook extends ProphetsBook

  case object Joshua extends EarlyProphetsBook
  case object Judges extends EarlyProphetsBook
  case object SamuelI extends EarlyProphetsBook { override def name: String = "I Samuel" }
  case object SamuelII extends EarlyProphetsBook { override def name: String = "II Samuel" }
  case object KingsI extends EarlyProphetsBook { override def name: String = "I Kings" }
  case object KingsII extends EarlyProphetsBook { override def name: String = "II Kings" }

  val earlyProphets: Seq[ProphetsBook] = Seq(Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII)

  trait LateProphetsBook extends ProphetsBook

  case object Isaiah extends LateProphetsBook
  case object Jeremiah extends LateProphetsBook
  case object Ezekiel extends LateProphetsBook

  <!-- תרי עשר -->
  trait TreiAsarBook extends LateProphetsBook

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

  trait WritingsBook extends Book

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

  val nach: Seq[Book] = prophets ++ writings

  override val values: Seq[Book] = chumash ++ nach

  private final case class TanachMetadata(names: Names, chapterElements: Seq[Elem], weekElements: Seq[Elem])

  // We do not need preparsed metadata once it is parsed, so the reference is week:
  private var toMetadata: WeakReference[Map[Book, TanachMetadata]] = new WeakReference(loadMetadata)

  private def getToMetadata: Map[Book, TanachMetadata] = {
    toMetadata.get.fold {
      val result = loadMetadata
      toMetadata = new WeakReference(result)
      result
    } { result => result }
  }

  private def loadMetadata: Map[Book, TanachMetadata] = Metadata.load(
    values = values,
    obj = this,
    resourceName = "Tanach",
    rootElementName = "metadata",
    elementName = "book"
  ).map { case (book, metadata) =>
    metadata.attributes.close()
    val (chapterElements: Seq[Elem], weekElements: Seq[Elem]) = XML.span(metadata.elements, "chapter", "week")
    if (!book.isInstanceOf[ChumashBook]) XML.checkNoMoreElements(weekElements)
    book -> TanachMetadata(metadata.names, chapterElements, weekElements)
  }

  def main(args: Array[String]): Unit = {
    println(Joshua.names)
  }
}
