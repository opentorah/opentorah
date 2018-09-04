package org.podval.calendar.generate.tanach

object Tanach {
  abstract class BookStructure(
    val names: Names,
    val chapters: Array[Int]
  )

  final class ChumashBookStructure(
    names: Names,
    chapters: Array[Int],
    val weeks: Seq[Parsha.Structure]
  ) extends BookStructure(names, chapters)

  final class NachBookStructure(
    names: Names,
    chapters: Array[Int]
  ) extends BookStructure(names, chapters)

  sealed trait Book {
    def name: String = getClass.getSimpleName.replace("$", "")

    def structure: BookStructure
  }

  trait ChumashBook extends Book {
    final override def structure: ChumashBookStructure = TanachParser.forBook(this)
  }

  trait NachBook extends Book {
    final override def structure: NachBookStructure = TanachParser.forBook(this)
  }

  case object Genesis extends ChumashBook
  case object Exodus extends ChumashBook
  case object Leviticus extends ChumashBook
  case object Numbers extends ChumashBook
  case object Deuteronomy extends ChumashBook

  case object Joshua extends NachBook
  case object Judges extends NachBook
  case object SamuelI extends NachBook { override def name: String = "Samuel I" }
  case object SamuelII extends NachBook { override def name: String = "Samuel II" }
  case object KingsI extends NachBook { override def name: String = "Kings I" }
  case object KingsII extends NachBook { override def name: String = "Kings II" }
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
  case object SongOfSongs extends NachBook { override def name: String = "Song of Songs" }
  case object Ruth extends NachBook
  case object Lamentations extends NachBook
  case object Ecclesiastes extends NachBook
  case object Esther extends NachBook
  case object Daniel extends NachBook
  case object Ezra extends NachBook
  case object Nehemiah extends NachBook
  case object ChroniclesI extends NachBook { override def name: String = "Chronicles I" }
  case object ChroniclesII extends NachBook { override def name: String = "Chronicles II" }

  val chumash: Seq[ChumashBook] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  val prophets: Seq[NachBook] = Seq(
    Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII, Isaiah, Jeremiah, Ezekiel,
    Hosea, Joel, Amos, Obadiah, Jonah, Micah, Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi)

  val writings: Seq[NachBook] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)

  val nach: Seq[NachBook] = prophets ++ writings

  val all: Seq[Book] = chumash ++ nach

  def forName(name: String): Option[Book] = TanachParser.forBookName(name)

  def main(args: Array[String]): Unit = {
    val genesis = Genesis.structure
    val genesisWeek = Parsha.Bereishis.structure
    val deuteronomy = forName("Devarim").get.structure
    val z = 0
  }
}
