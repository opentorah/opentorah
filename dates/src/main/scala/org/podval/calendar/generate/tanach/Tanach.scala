package org.podval.calendar.generate.tanach

object Tanach {

  sealed trait Book {
    def name: String = Util.className(this)

    def structure: BookStructure

    final def names: Names = structure.names
  }

  abstract class BookStructure(
    val book: Book,
    val names: Names,
    val chapters: Chapters
  )

  trait ChumashBook extends Book {
    final override def structure: ChumashBookStructure = chumashStructure(this)
  }

  final class ChumashBookStructure(
    book: ChumashBook,
    names: Names,
    chapters: Chapters,
    val weeks: Map[Parsha, Parsha.Structure]
  ) extends BookStructure(book, names, chapters)

  trait NachBook extends Book {
    final override def structure: NachBookStructure = nachStructure(this)
  }

  final class NachBookStructure(
    book: NachBook,
    names: Names,
    chapters: Chapters
  ) extends BookStructure(book, names, chapters)

  case object Genesis extends ChumashBook
  case object Exodus extends ChumashBook
  case object Leviticus extends ChumashBook
  case object Numbers extends ChumashBook
  case object Deuteronomy extends ChumashBook

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  trait ProphetsBook extends NachBook

  case object Joshua extends ProphetsBook
  case object Judges extends ProphetsBook
  case object SamuelI extends ProphetsBook { override def name: String = "Samuel I" }
  case object SamuelII extends ProphetsBook { override def name: String = "Samuel II" }
  case object KingsI extends ProphetsBook { override def name: String = "Kings I" }
  case object KingsII extends ProphetsBook { override def name: String = "Kings II" }
  case object Isaiah extends ProphetsBook
  case object Jeremiah extends ProphetsBook
  case object Ezekiel extends ProphetsBook

  <!-- תרי עשר -->
  trait TreiAsarBook extends ProphetsBook

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

  val prophets: Seq[ProphetsBook] =
    Seq(Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII, Isaiah, Jeremiah, Ezekiel) ++ treiAsar

  trait WritingsBook extends NachBook

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

  val nach: Seq[NachBook] = prophets ++ writings

  val all: Seq[Book] = chumash ++ nach

  private val (
    chumashStructure: Map[ChumashBook, ChumashBookStructure],
    nachStructure: Map[NachBook, NachBookStructure]
  ) = TanachParser.parse

  def forChumashName(name: String): Option[ChumashBook] = chumashStructure.find(_._2.names.has(name)).map(_._1)
  def forNachName(name: String): Option[NachBook] = nachStructure.find(_._2.names.has(name)).map(_._1)
  def forName(name: String): Option[Book] = forChumashName(name).orElse(forNachName(name))

  def main(args: Array[String]): Unit = {
    val genesis = Genesis.structure
    val genesisWeek = Parsha.Bereishis.structure
    val deuteronomy = forName("Devarim").get.structure
    val z = 0
  }
}
