package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{MainMetadata, MetadataLoader, Named, Names, PreparsedMetadata, XML}

object Nach extends MainMetadata with MetadataLoader {
  trait Book extends KeyBase

  override type Key = Book

  final class BookStructure(
    override val names: Names,
    val chapters: Chapters
  ) extends Named.NamedBase

  override type Metadata = BookStructure

  trait ProphetsBook extends Book

  trait EarlyProphetsBook extends ProphetsBook

  case object Joshua extends EarlyProphetsBook
  case object Judges extends EarlyProphetsBook
  case object SamuelI extends EarlyProphetsBook { override def name: String = "Samuel I" }
  case object SamuelII extends EarlyProphetsBook { override def name: String = "Samuel II" }
  case object KingsI extends EarlyProphetsBook { override def name: String = "Kings I" }
  case object KingsII extends EarlyProphetsBook { override def name: String = "Kings II" }

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
  case object ChroniclesI extends WritingsBook { override def name: String = "Chronicles I" }
  case object ChroniclesII extends WritingsBook { override def name: String = "Chronicles II" }

  val writings: Seq[WritingsBook] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)

  override val values: Seq[Book] = prophets ++ writings

  protected override def elementName: String = "book"

  protected override def parseMetadata(book: Book, metadata: PreparsedMetadata): BookStructure = {
    metadata.attributes.close()
    val chapterElements = XML.span(metadata.elements, "chapter")

    new BookStructure(
      metadata.names,
      chapters = Chapters(chapterElements)
    )
  }
}
