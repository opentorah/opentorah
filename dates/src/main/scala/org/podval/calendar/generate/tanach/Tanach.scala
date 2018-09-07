package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{Names, Metadata, WithMetadata}

object Tanach {

  sealed trait Book[M <: BookStructure] extends WithMetadata[M]

  abstract class BookStructure(
    // TODO remove?
    book: Book[_],
    val names: Names,
    val chapters: Chapters
  ) extends Metadata

  trait ChumashBook extends Book[ChumashBookStructure] {
    final override def metadata: ChumashBookStructure = chumashStructure(this)
  }

  final class ChumashBookStructure(
    book: ChumashBook,
    names: Names,
    chapters: Chapters,
    val weeks: Map[Parsha, Parsha.Structure]
  ) extends BookStructure(book, names, chapters)

  trait NachBook extends Book[NachBookStructure] {
    final override def metadata: NachBookStructure = nachStructure(this)
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

  val all: Seq[Book[_]] = chumash ++ nach

  private val (
    chumashStructure: Map[ChumashBook, ChumashBookStructure],
    nachStructure: Map[NachBook, NachBookStructure]
  ) = TanachParser.parse(this)

  def forChumashName(name: String): Option[ChumashBook] = chumashStructure.find(_._2.names.has(name)).map(_._1)
  def forNachName(name: String): Option[NachBook] = nachStructure.find(_._2.names.has(name)).map(_._1)
  def forName(name: String): Option[Book[_]] = forChumashName(name).orElse(forNachName(name))

  def main(args: Array[String]): Unit = {
    def printSpans(spans: Seq[Span]): Unit = spans.zipWithIndex.foreach { case (span, index) =>
      println(s"${index+1}: $span")
    }

/////    printSpans(Parsha.Mattos.structure.days)
//    printSpans(Parsha.Mattos.structure.daysCombined)
    val x = Parsha.Mattos.metadata
    printSpans(Parsha.Mattos.metadata.daysCombinedCustom("Ashkenaz"))
    println()
//    printSpans(Parsha.Masei.structure.days)
//    printSpans(Parsha.Masei.structure.daysCustom("Ashkenaz"))
//    printSpans(Parsha.Masei.structure.daysCombined)
    println()
  }
}
