package org.podval.calendar.generate.tanach

object Tanach {
  final class Verse(val book: Book, val chapter: Int, val verse: Int) extends Ordered[Verse] {
    require(chapter > 0)
    require(verse > 0)

    def isSameBook(that: Verse): Boolean = this.book == that.book

    override def compare(that: Verse): Int = {
      require(isSameBook(that))
      val result = this.chapter - that.chapter
      if (result != 0) result else this.verse - that.verse
    }
  }

  final class Fragment(val from: Verse, val to: Verse) {
    require(from.isSameBook(to))
    require(from <= to)

    def book: Book = from.book

    def contains(verse: Verse): Boolean = (from <= verse) && (verse <= to)
  }

  final class Chapters(book: Book, chapters: Array[Int]) {
    def length(chapter: Int): Int = chapters(chapter-1)

    def next(verse: Verse): Option[Verse] = {
      if (verse.verse < length(verse.chapter))
        Some(new Verse(verse.book, verse.chapter, verse.verse+1))
      else if (verse.chapter+1 <= chapters.length)
        Some(new Verse(verse.book, verse.chapter+1, 1))
      else
        None
    }

    def prev(verse: Verse): Option[Verse] = {
      if (verse.verse > 1)
        Some(new Verse(verse.book, verse.chapter, verse.verse-1))
      else if (verse.chapter-1 >= 1)
        Some(new Verse(verse.book, verse.chapter-1, length(verse.chapter-1)))
      else
        None
    }

    def first: Verse = new Verse(book, 1, 1)

    def last: Verse = new Verse(book, chapters.length, length(chapters.length))

    def contains(verse: Verse): Boolean = (first <= verse) && (verse <= last)
  }

  sealed trait Book {
    def name: String = Util.className(this)

    def structure: BookStructure

    final def names: Names = structure.names
  }

  abstract class BookStructure(
    val names: Names,
    val chapters: Array[Int]
  )

  trait ChumashBook extends Book {
    final override def structure: ChumashBookStructure = chumashStructure(this)
  }

  final class ChumashBookStructure(
    names: Names,
    chapters: Array[Int],
    val weeks: Seq[Parsha.Structure]
  ) extends BookStructure(names, chapters)

  trait NachBook extends Book {
    final override def structure: NachBookStructure = nachStructure(this)
  }

  final class NachBookStructure(
    names: Names,
    chapters: Array[Int]
  ) extends BookStructure(names, chapters)

  trait ProphetsBook extends NachBook
  trait TreiAsarBook extends ProphetsBook

  trait WritingsBook extends NachBook

  case object Genesis extends ChumashBook
  case object Exodus extends ChumashBook
  case object Leviticus extends ChumashBook
  case object Numbers extends ChumashBook
  case object Deuteronomy extends ChumashBook

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

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  val prophets: Seq[NachBook] = Seq(
    Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII, Isaiah, Jeremiah, Ezekiel,
    Hosea, Joel, Amos, Obadiah, Jonah, Micah, Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi)

  val writings: Seq[NachBook] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
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
