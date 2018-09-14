package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{HasNames, Names, WithMetadata, LanguageSpec, Language}

object Tanach {

  sealed trait Book[K <: WithMetadata[K, M], M <: BookStructure] extends WithMetadata[K, M] { this: K => }

  type TanachBook = Book[_, _]

  abstract class BookStructure(
    // TODO remove?
    book: TanachBook,
    override val names: Names,
    val chapters: Chapters
  ) extends HasNames

  trait ChumashBook extends Book[ChumashBook, ChumashBookStructure] {
    final override def toMetadata: Map[ChumashBook, ChumashBookStructure] = chumashStructure
  }

  final class ChumashBookStructure(
    book: ChumashBook,
    names: Names,
    chapters: Chapters,
    val weeks: Map[Parsha, Parsha.Structure]
  ) extends BookStructure(book, names, chapters)

  trait NachBook extends Book[NachBook, NachBookStructure] {
    final override def toMetadata: Map[NachBook, NachBookStructure] = nachStructure
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

  val all: Seq[TanachBook] = chumash ++ nach

  private val (
    chumashStructure: Map[ChumashBook, ChumashBookStructure],
    nachStructure: Map[NachBook, NachBookStructure]
  ) = TanachMetadataParser.parse(this)

  def forChumashName(name: String): Option[ChumashBook] = chumash.find(_.names.has(name))
  def forNachName(name: String): Option[NachBook] = nach.find(_.names.has(name))
  def forName(name: String): Option[TanachBook] = forChumashName(name).orElse(forNachName(name))

  private def printHaftarahList(custom: Custom, spec: LanguageSpec): Unit = {
    println(custom.toString(spec))
    for (parsha <- Parsha.all) {
      val haftarah: Haftarah = Haftarah.forParsha(parsha)
      val customEffective: Custom = Custom.find(haftarah.customs, custom)
      val customEffectiveString: String =
        if (customEffective == custom) ""
        else " [" + customEffective.toString(spec)  + "]"
      val spans: Seq[NachSpan] = haftarah.customs(customEffective)
      println(parsha.toString(spec) + customEffectiveString + ": " + NachSpan.toString(spans, spec))
    }
  }

  def main(args: Array[String]): Unit = {
    def printSpans(spans: Seq[Span]): Unit = spans.zipWithIndex.foreach { case (span, index) =>
      println(s"${index+1}: $span")
    }

//    printSpans(Parsha.Mattos.metadata.daysCombined(Custom.Ashkenaz))


    printHaftarahList(Custom.Ashkenaz, LanguageSpec(Language.Hebrew))
    println()
    printHaftarahList(Custom.Djerba, LanguageSpec(Language.Russian))
  }
}
