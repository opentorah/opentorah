package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.tanach.Tanach.ChumashBook
import org.podval.judaica.metadata.tanach.SpanParser.{NumberedSpan, SpanParsed}
import org.podval.judaica.metadata.{LanguageSpec, MainMetadata, Named, Names, PreparsedMetadata, SubresourceLoader, XML}

import scala.xml.Elem

object Parsha extends MainMetadata with SubresourceLoader {
  sealed trait Parsha extends KeyBase {
    def book: ChumashBook

    final def combines: Boolean = Parsha.combinableAll.contains(this)

    final override def toString: String = toString(LanguageSpec.empty)
  }

  override type Key = Parsha

  final class Structure(
    override val names: Names,
    val span: Span,
    val days: Custom.Of[Seq[Span]],
    val daysCombined: Custom.Of[Seq[Span]],
    val maftir: Span,
    val aliyot: Seq[Span]
  ) extends Named.NamedBase

  override type BindableMetadata = Combined

  override type Metadata = Structure

  final class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  trait GenesisParsha extends Parsha { final override def book: ChumashBook = Tanach.Genesis }

  case object Bereishis extends GenesisParsha
  case object Noach extends GenesisParsha
  case object LechLecha extends GenesisParsha { override def name: String = "Lech Lecha" }
  case object Vayeira extends GenesisParsha
  case object ChayeiSarah extends GenesisParsha { override def name: String = "Chayei Sarah" }
  case object Toldos extends GenesisParsha
  case object Vayeitzei extends GenesisParsha
  case object Vayishlach extends GenesisParsha
  case object Vayeishev extends GenesisParsha
  case object Mikeitz extends GenesisParsha
  case object Vayigash extends GenesisParsha
  case object Vayechi extends GenesisParsha

  val genesis: Seq[Parsha] = Seq(Bereishis, Noach, LechLecha, Vayeira, ChayeiSarah, Toldos,
    Vayeitzei, Vayishlach, Vayeishev, Mikeitz, Vayigash, Vayechi)

  trait ExodusParsha extends Parsha { final override def book: ChumashBook = Tanach.Exodus }

  case object Shemos extends ExodusParsha
  case object Va_eira extends ExodusParsha { override def name: String = "Va'eira" }
  case object Bo extends ExodusParsha
  case object Beshalach extends ExodusParsha
  case object Yisro extends ExodusParsha
  case object Mishpatim extends ExodusParsha
  case object Terumah extends ExodusParsha
  case object Tetzaveh extends ExodusParsha
  case object KiSisa extends ExodusParsha { override def name: String = "Ki Sisa" }
  case object Vayakhel extends ExodusParsha
  case object Pekudei extends ExodusParsha

  val exodus: Seq[Parsha] = Seq(Shemos, Va_eira, Bo, Beshalach, Yisro, Mishpatim, Terumah,
    Tetzaveh, KiSisa, Vayakhel, Pekudei)

  trait LeviticusParsha extends Parsha { final override def book: ChumashBook = Tanach.Leviticus }

  case object Vayikra extends LeviticusParsha
  case object Tzav extends LeviticusParsha
  case object Shemini extends LeviticusParsha
  case object Tazria extends LeviticusParsha
  case object Metzora extends LeviticusParsha
  case object Acharei extends LeviticusParsha
  case object Kedoshim extends LeviticusParsha
  case object Emor extends LeviticusParsha
  case object Behar extends LeviticusParsha
  case object Bechukosai extends LeviticusParsha

  val leviticus: Seq[Parsha] = Seq(Vayikra, Tzav, Shemini, Tazria, Metzora, Acharei, Kedoshim, Emor, Behar, Bechukosai)

  trait NumbersParsha extends Parsha { final override def book: ChumashBook = Tanach.Numbers }

  case object Bemidbar extends NumbersParsha
  case object Nasso extends NumbersParsha
  case object Beha_aloscha extends NumbersParsha { override def name: String = "Beha'aloscha" }
  case object Shelach extends NumbersParsha
  case object Korach extends NumbersParsha
  case object Chukas extends NumbersParsha
  case object Balak extends NumbersParsha
  case object Pinchas extends NumbersParsha
  case object Mattos extends NumbersParsha
  case object Masei extends NumbersParsha

  val numbers: Seq[Parsha] = Seq(Bemidbar, Nasso, Beha_aloscha, Shelach, Korach, Chukas, Balak, Pinchas, Mattos, Masei)

  trait DeutoronomyParsha extends Parsha { final override def book: ChumashBook = Tanach.Deuteronomy }

  case object Devarim extends DeutoronomyParsha
  case object Va_eschanan extends DeutoronomyParsha { override def name: String = "Va'eschanan" }
  case object Eikev extends DeutoronomyParsha
  case object Re_eh extends DeutoronomyParsha { override def name: String = "Re'eh" }
  case object Shoftim extends DeutoronomyParsha
  case object KiSeitzei extends DeutoronomyParsha { override def name: String = "Ki Seitzei" }
  case object KiSavo extends DeutoronomyParsha { override def name: String = "Ki Savo" }
  case object Nitzavim extends DeutoronomyParsha
  case object Vayeilech extends DeutoronomyParsha
  case object Haazinu extends DeutoronomyParsha
  case object VezosHaberachah extends DeutoronomyParsha { override def name: String = "Vezos Haberachah" }

  val deuteronomy: Seq[Parsha] = Seq(Devarim, Va_eschanan, Eikev, Re_eh, Shoftim, KiSeitzei, KiSavo,
    Nitzavim, Vayeilech, Haazinu, VezosHaberachah)

  // TODO add half-parshiot for the Dardaki custom

  final override val values: Seq[Parsha] = genesis ++ exodus ++ leviticus ++ numbers ++ deuteronomy

  // Rules of combining; affect the WeeklyReading.
  // TODO deal with alternative customs of what and in what sequence combines?
  final val combinableFromBereishisToVayikra: Seq[Parsha] = Seq(Vayakhel)
  // TODO see #56; Magen Avraham 428:4 (6);
  // Reversing the priorities here currently affects only non-leap regular years with Rosh
  // Hashanah on Thursday (and Pesach on Shabbat).
  final val combinableFromVayikraToBemidbar: Seq[Parsha] = Seq(Tazria, Acharei, Behar)
  final val combinableFromBemidbarToVa_eschanan: Seq[Parsha] = Seq(Mattos, Chukas)
  final val combinableFromVa_eschanan: Seq[Parsha] = Seq(Nitzavim)

  final val combinableAll: Set[Parsha] = (combinableFromBereishisToVayikra ++ combinableFromVayikraToBemidbar ++
    combinableFromBemidbarToVa_eschanan ++ combinableFromVa_eschanan).toSet

  override def toMetadata: Parsha => Structure = parsha => parsha.book.weeks(parsha)

  override protected def elementName: String = "week"

  def parse(book: Tanach.ChumashBook, elements: Seq[Elem]): Map[Parsha, Structure] = {
    val chapters = book.chapters
    val preparsed: Seq[Preparsed] = elements.map(element => loadSubresource(element)).map(preparse)
    val spans: Seq[Span] = SpanParser.setImpliedTo(preparsed.map(_.span), chapters.full, chapters)
    require(spans.length == preparsed.length)
    val parsed: Seq[Parsed] = preparsed.zip(spans).map { case (week, span) => week.parse(span, chapters) }

    bind(
      keys = book.parshiot,
      metadatas = combine(parsed),
      parse = (parsha: Parsha, week: Combined) => week.squash(parsha, chapters)
    )
  }

  private def preparse(metadata: PreparsedMetadata): Preparsed = {
    val result = new Preparsed(
      span = SpanParser.parseSpan(metadata.attributes),
      names = metadata.names,
      elements = metadata.elements
    )
    metadata.attributes.close()
    result
  }

  private final class Preparsed(
    val span: SpanParsed,
    val names: Names,
    val elements: Seq[Elem]
  ) {
    def parse(span: Span, chapters: Chapters): Parsed = {
      val (aliyahElements, dayElements, maftirElements) = XML.span(elements,
        "aliyah", "day", "maftir")
      require(maftirElements.length == 1)

      def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[NumberedSpan]] =
        days.groupBy(_.custom).mapValues(days => days.map(_.span))

      val (days: Seq[DayParsed], daysCombined: Seq[DayParsed]) = dayElements.map(parseDay).partition(!_.isCombined)
      val daysResult: Custom.Sets[Seq[Span]] = processDays(byCustom(days), span, chapters)

      val aliyot: Seq[NumberedSpan] = aliyahElements.map(element => SpanParser.parseNumberedSpan(element, "aliyah"))
      // TODO QUESTION if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
      // TODO QUESTION if the parshiot combine, does it affect those small aliyot?
      val aliyotSpan: Span = Span(span.from, aliyot.last.span.to.getOrElse(Custom.common(daysResult).head.to))
      val aliyotWithImplied1: Seq[NumberedSpan] = SpanParser.addImplied1(aliyot, aliyotSpan, chapters)
      val aliyotResult: Seq[Span] =
        SpanParser.setImpliedToCheckAndDropNumbers(aliyotWithImplied1, 3, aliyotSpan, chapters)

      val maftir: SpanParsed = SpanParser.parseSpan(maftirElements.head, "maftir")
      val maftirResult: Span = SpanParser.setImpliedTo(
        Seq(maftir),
        Span(maftir.from, maftir.to.getOrElse(span.to)),
        chapters
      ).head

      Parsed(
        names,
        span = span,
        days = daysResult,
        daysCombined = byCustom(daysCombined),
        aliyot = aliyotResult,
        maftir = maftirResult
      )
    }
  }

  private def combine(weeks: Seq[Parsed]): Seq[Combined] = weeks match {
    case week1 :: week2 :: tail => week1.combine(week2.daysCombined, week2.span) +: combine(week2 +: tail)
    case week :: Nil => Seq(week.combine(Map.empty, Span(Verse(1, 1), Verse(1, 1)))) // The Span will never be used!
    case Nil => Nil
  }

  private final case class Parsed(
    names: Names,
    span: Span,
    days: Custom.Sets[Seq[Span]],
    daysCombined: Custom.Sets[Seq[NumberedSpan]],
    aliyot: Seq[Span],
    maftir: Span
  ) {
    def combine(
      daysCombinedNext: Custom.Sets[Seq[NumberedSpan]],
      spanNext: Span
    ): Combined = new Combined(
      names = names,
      span = span,
      days = days,
      daysCombined = daysCombined,
      spanNext = spanNext,
      daysCombinedNext = daysCombinedNext,
      maftir = maftir,
      aliyot = aliyot
    )
  }

  final class Combined(
    val names: Names,
    val span: Span,
    val days: Custom.Sets[Seq[Span]],
    val daysCombined: Custom.Sets[Seq[NumberedSpan]],
    val spanNext: Span,
    val daysCombinedNext: Custom.Sets[Seq[NumberedSpan]],
    val aliyot: Seq[Span],
    val maftir: Span
  ) extends Named.HasNames {
    def squash(parsha: Parsha, chapters: Chapters): Parsha.Structure = {
      def combine: Custom.Sets[Seq[Span]] = {
        // TODO Use defaults from days?
        val combinedFull = daysCombinedNext ++ daysCombined.map { case (customs, value) =>
          (customs, value ++ daysCombinedNext.getOrElse(customs, Seq.empty))
        }
        processDays(combinedFull, chapters.merge(span, spanNext), chapters)
      }

      new Parsha.Structure(
        names = names,
        span = span,
        days = Custom.denormalize(days),
        daysCombined = if (!parsha.combines) Map.empty else Custom.denormalize(combine),
        maftir = maftir,
        aliyot = aliyot
      )
    }
  }

  private final class DayParsed(
    val span: NumberedSpan,
    val custom: Set[Custom.Custom],
    val isCombined: Boolean
  )

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day")
    val result = new DayParsed(
      span = SpanParser.parseNumberedSpan(attributes),
      custom = attributes.get("custom").fold[Set[Custom.Custom]](Set(Custom.Common))(Custom.parse),
      isCombined = attributes.doGetBoolean("combined")
    )
    attributes.close()
    result
  }

  private def processDays(
    days: Custom.Sets[Seq[NumberedSpan]],
    span: Span,
    chapters: Chapters
  ): Custom.Sets[Seq[Span]] = {
    val withImplied1 = SpanParser.addImplied1(Custom.common(days), span, chapters)

    days.mapValues { spans: Seq[NumberedSpan] =>
      val overlayedSpans = SpanParser.overlaySpans(withImplied1, spans)
      SpanParser.setImpliedToCheckAndDropNumbers(overlayedSpans, 7, span, chapters)
    }
  }
}
