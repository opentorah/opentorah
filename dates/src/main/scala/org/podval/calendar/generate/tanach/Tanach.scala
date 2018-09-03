package org.podval.calendar.generate.tanach

object Tanach {
  abstract class BookStructure(
    val names: Names,
    val chapters: Array[Int]
  )

  final class ChumashBookStructure(
    names: Names,
    chapters: Array[Int],
    val weeks: Seq[ParshaStructure]
  ) extends BookStructure(names, chapters)

  final class NachBookStructure(
    names: Names,
    chapters: Array[Int]
  ) extends BookStructure(names, chapters)

  final class ParshaStructure(
    val names: Names,
    val fromChapter: Int,
    val fromVerse: Int,
    val toChapter: Int,
    val toVerse: Int,
    val maftir: Maftir
  ) {
    require(fromChapter > 0)
    require(fromVerse > 0)
  }

  final class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  // TODO add custom and combined
  // TODO check against Parsha what can be combined
  final class Day(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  // TODO special Maftir has reference to the book; generalize?
  final class Maftir(
    val fromChapter: Int,
    val fromVerse: Int,
    val toChapter: Int,
    val toVerse: Int
  )

  sealed trait Book {
    final def name: String = {
      val result = getClass.getSimpleName.replace("$", "")
      if (result.endsWith("II")) result.replace("II", " II") else
      if (result.endsWith("I")) result.replace("I", " I") else
      if (result == "SongOfSongs") "Song of Songs" else
        result
    }

    def structure: BookStructure
  }

  trait ChumashBook extends Book {
    final override lazy val structure: ChumashBookStructure = structureForName(name).asInstanceOf[ChumashBookStructure]
  }

  trait NachBook extends Book {
    final override lazy val structure: NachBookStructure = structureForName(name).asInstanceOf[NachBookStructure]
  }

  case object Genesis extends ChumashBook
  case object Exodus extends ChumashBook
  case object Leviticus extends ChumashBook
  case object Numbers extends ChumashBook
  case object Deuteronomy extends ChumashBook

  case object Joshua extends NachBook
  case object Judges extends NachBook
  case object SamuelI extends NachBook
  case object SamuelII  extends NachBook
  case object KingsI extends NachBook
  case object KingsII  extends NachBook
  case object Isaiah extends NachBook
  case object Jeremiah extends NachBook
  case object Ezekiel extends NachBook

  <!-- תרי עשר -->
  case object Hosea extends NachBook
  case object Joel extends NachBook
  case object Amos extends NachBook
  case object Obadiah extends NachBook
  case object Jonah extends NachBook
  case object Micah extends NachBook
  case object Nahum extends NachBook
  case object Habakkuk extends NachBook
  case object Zephaniah extends NachBook
  case object Haggai extends NachBook
  case object Zechariah extends NachBook
  case object Malachi extends NachBook

  case object Psalms extends NachBook
  case object Proverbs extends NachBook
  case object Job extends NachBook
  case object SongOfSongs extends NachBook
  case object Ruth extends NachBook
  case object Lamentations extends NachBook
  case object Ecclesiastes extends NachBook
  case object Esther extends NachBook
  case object Daniel extends NachBook
  case object Ezra extends NachBook
  case object Nehemiah extends NachBook
  case object ChroniclesI extends NachBook
  case object ChroniclesII extends NachBook

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  val nach: Seq[NachBook] = Seq(
    Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII, Isaiah, Jeremiah, Ezekiel,
    Hosea, Joel, Amos, Obadiah, Jonah, Micah, Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi,
    Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes, Esther,
    Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)

  val all: Seq[Book] = chumash ++ nach

  private val structures: Seq[BookStructure] = TanachParser.parse
  private val structure2book: Map[BookStructure, Book] = all.map(book => book.structure -> book).toMap
  require(structures.toSet == structure2book.keySet)

  def structureForName(name: String): BookStructure = {
    val result = structures.filter(_.names.has(name))
    require(result.nonEmpty, s"No structure for $name")
    require(result.length == 1)
    result.head
  }

  def bookForName(name: String): Book = structure2book(structureForName(name))

  def main(args: Array[String]): Unit = {
    val genesis = Genesis.structure
    val deuteronomy = bookForName("Devarim")
    val z = 0
  }
}
