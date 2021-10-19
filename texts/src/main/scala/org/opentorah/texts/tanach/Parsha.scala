package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Names, WithNumber}
import org.opentorah.store.{By, Pure, Store}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}
import Tanach.Chumash

enum Parsha(val book: Chumash, nameOverride: Option[String] = None) extends
  HasName(nameOverride),
  HasName.Enum,
  Pure[?] derives CanEqual:

  case Bereishis       extends Parsha(Chumash.Genesis)
  case Noach           extends Parsha(Chumash.Genesis)
  case LechLecha       extends Parsha(Chumash.Genesis, nameOverride = Some("Lech Lecha"))
  case Vayeira         extends Parsha(Chumash.Genesis)
  case ChayeiSarah     extends Parsha(Chumash.Genesis, nameOverride = Some("Chayei Sarah"))
  case Toldos          extends Parsha(Chumash.Genesis)
  case Vayeitzei       extends Parsha(Chumash.Genesis)
  case Vayishlach      extends Parsha(Chumash.Genesis)
  case Vayeishev       extends Parsha(Chumash.Genesis)
  case Mikeitz         extends Parsha(Chumash.Genesis)
  case Vayigash        extends Parsha(Chumash.Genesis)
  case Vayechi         extends Parsha(Chumash.Genesis)

  case Shemos          extends Parsha(Chumash.Exodus)
  case Va_eira         extends Parsha(Chumash.Exodus, nameOverride = Some("Va'eira"))
  case Bo              extends Parsha(Chumash.Exodus)
  case Beshalach       extends Parsha(Chumash.Exodus)
  case Yisro           extends Parsha(Chumash.Exodus)
  case Mishpatim       extends Parsha(Chumash.Exodus)
  case Terumah         extends Parsha(Chumash.Exodus)
  case Tetzaveh        extends Parsha(Chumash.Exodus)
  case KiSisa          extends Parsha(Chumash.Exodus, nameOverride = Some("Ki Sisa"))
  case Vayakhel        extends Parsha(Chumash.Exodus)
  case Pekudei         extends Parsha(Chumash.Exodus)

  case Vayikra         extends Parsha(Chumash.Leviticus)
  case Tzav            extends Parsha(Chumash.Leviticus)
  case Shemini         extends Parsha(Chumash.Leviticus)
  case Tazria          extends Parsha(Chumash.Leviticus)
  case Metzora         extends Parsha(Chumash.Leviticus)
  case Acharei         extends Parsha(Chumash.Leviticus)
  case Kedoshim        extends Parsha(Chumash.Leviticus)
  case Emor            extends Parsha(Chumash.Leviticus)
  case Behar           extends Parsha(Chumash.Leviticus)
  case Bechukosai      extends Parsha(Chumash.Leviticus)

  case Bemidbar        extends Parsha(Chumash.Numbers)
  case Nasso           extends Parsha(Chumash.Numbers)
  case Beha_aloscha    extends Parsha(Chumash.Numbers, nameOverride = Some("Beha'aloscha"))
  case Shelach         extends Parsha(Chumash.Numbers)
  case Korach          extends Parsha(Chumash.Numbers)
  case Chukas          extends Parsha(Chumash.Numbers)
  case Balak           extends Parsha(Chumash.Numbers)
  case Pinchas         extends Parsha(Chumash.Numbers)
  case Mattos          extends Parsha(Chumash.Numbers)
  case Masei           extends Parsha(Chumash.Numbers)

  case Devarim         extends Parsha(Chumash.Deuteronomy)
  case Va_eschanan     extends Parsha(Chumash.Deuteronomy, nameOverride = Some("Va'eschanan"))
  case Eikev           extends Parsha(Chumash.Deuteronomy)
  case Re_eh           extends Parsha(Chumash.Deuteronomy, nameOverride = Some("Re'eh"))
  case Shoftim         extends Parsha(Chumash.Deuteronomy)
  case KiSeitzei       extends Parsha(Chumash.Deuteronomy, nameOverride = Some("Ki Seitzei"))
  case KiSavo          extends Parsha(Chumash.Deuteronomy, nameOverride = Some("Ki Savo"))
  case Nitzavim        extends Parsha(Chumash.Deuteronomy)
  case Vayeilech       extends Parsha(Chumash.Deuteronomy)
  case Haazinu         extends Parsha(Chumash.Deuteronomy)
  case VezosHaberachah extends Parsha(Chumash.Deuteronomy, nameOverride = Some("Vezos Haberachah"))

  private def metadata: Parsha.ParshaMetadata = book.metadata.forParsha(this)

  final override def names: Names = metadata.names

  final def span: Span = metadata.span

  final def days: Torah.Customs = metadata.days

  final def daysCombined: Option[Torah.Customs] = metadata.daysCombined

  final def getDaysCombined: Torah.Customs =
    require(this.combines)
    daysCombined.get

  final def aliyot: Torah = metadata.aliyot

  final def maftir: Torah.Maftir = metadata.maftir

  final def combines: Boolean = Parsha.combinable.contains(this)

  final def haftarah: Haftarah.Customs = Haftarah.haftarah(this).map(_.from(this))

  override def storesPure: Seq[By[?]] = Seq(
    Chapters.ByChapter(span, book.chapters)
  )

object Parsha extends Names.Loader[Parsha], HasValues.Distance[Parsha]:
  override val valuesSeq: Seq[Parsha] = values.toIndexedSeq

  def forChumash(book: ChumashBook): Seq[Parsha] = valuesSeq.filter(_.book == book)

  // Rules of combining; affect the WeeklyReading.
  val combinableFromBereishisToVayikra: Seq[Parsha] = Seq(Vayakhel)
  // Reversing the priorities here currently affects only non-leap regular years with Rosh
  // Hashanah on Thursday (and Pesach on Shabbat).
  val combinableFromVayikraToBemidbar: Seq[Parsha] = Seq(Tazria, Acharei, Behar)
  val combinableFromBemidbarToVa_eschanan: Seq[Parsha] = Seq(Mattos, Chukas)
  val combinableFromVa_eschanan: Seq[Parsha] = Seq(Nitzavim)

  val combinable: Set[Parsha] = (combinableFromBereishisToVayikra ++ combinableFromVayikraToBemidbar ++
    combinableFromBemidbarToVa_eschanan ++ combinableFromVa_eschanan).toSet

  final class ParshaMetadata(
    val parsha: Parsha,
    val names: Names,
    val span: Span,
    val days: Torah.Customs,
    val daysCombined: Option[Torah.Customs],
    val aliyot: Torah,
    val maftir: Torah.Maftir
  )

  final class Parsed(
    val parsha: Parsha,
    val names: Names,
    val span: SpanSemiResolved,
    val days: Custom.Sets[Seq[Torah.Numbered]],
    val daysCombined: Custom.Sets[Seq[Torah.Numbered]],
    val aliyot: Seq[Torah.Numbered],
    val maftir: SpanSemiResolved
  ):
    def resolve(
      parshaSpan: Span,
      daysCombined: Option[Torah.Customs]
    ): Parser[ParshaMetadata] =
      for
        days: Torah.Customs <- daysResolved(parshaSpan)
        aliyot: Torah <- aliyotResolved(parshaSpan, days)
        maftir: Torah.Maftir = maftirResolved(parshaSpan)
      yield ParshaMetadata(
        parsha,
        names,
        parshaSpan,
        days,
        daysCombined,
        aliyot,
        maftir
      )

    private def daysResolved(parshaSpan: Span): Parser[Torah.Customs] =
      Torah.processDays(parsha.book, days, parshaSpan)

    private def aliyotResolved(parshaSpan: Span, days: Torah.Customs): Parser[Torah] =
      val bookSpan = Torah.inBook(parsha.book,
        Span(
          parshaSpan.from,
          aliyot.last.what.to.getOrElse(days.common.spans.head.span.to)
        )
      )
      Torah.parseAliyot(bookSpan, aliyot, number = Some(3))

    private def maftirResolved(parshaSpan: Span): Torah.Maftir =
      val span = Span(maftir.from, maftir.to.getOrElse(parshaSpan.to))

      Torah.inBook(parsha.book,
        SpanSemiResolved.setImpliedTo(
          Seq(maftir),
          span,
          parsha.book.chapters
        ).head
      )

  final class WeekParsable(book: ChumashBook) extends Element[Parsed]("week"):
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed]:
      override def parser: Parser[Parsed] = Parsha.parser(book)
      override def unparser: Unparser[Parsed] = ???

  private def parser(book: ChumashBook): Parser[Parsed] = for
    names: Names <- Names.withoutDefaultNameParsable()
    span: SpanSemiResolved <- semiResolvedParser
    aliyot: Seq[Torah.Numbered] <- Aliyah.seq()
    daysParsed: Seq[DayParsed] <- DayParsed.seq()
    maftir: SpanSemiResolved <- Maftir.required()
    parsha: Parsha <- HasName.find[Parsha](book.parshiot, names)
  yield
    val (days: Seq[DayParsed], daysCombined: Seq[DayParsed]) = daysParsed.partition(!_.isCombined)
    Parsed(
      parsha,
      names,
      span,
      days = byCustom(days),
      daysCombined = byCustom(daysCombined),
      aliyot,
      maftir
    )

  private final class DayParsed(
    val span: Torah.Numbered,
    val custom: Set[Custom],
    val isCombined: Boolean
  )

  private object DayParsed extends Element[DayParsed]("day"):
    override def contentParsable: Parsable[DayParsed] = new Parsable[DayParsed]:
      override def parser: Parser[DayParsed] = for
        span: Torah.Numbered <- numberedParser
        custom: Set[Custom] <- Attribute("custom").optional().map(_.fold[Set[Custom]](Set(Custom.Common))(Custom.parse))
        isCombined: Boolean <- Attribute.BooleanAttribute("combined").optional().map(_.getOrElse(false))
      yield DayParsed(
        span,
        custom,
        isCombined
      )

      override def unparser: Unparser[DayParsed] = ???

  private def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[Torah.Numbered]] =
    Collections.mapValues(days.groupBy(_.custom))(days => days.map(_.span))

  object Aliyah extends Element[Torah.Numbered]("aliyah"):
    override def contentType: Element.ContentType = Element.ContentType.Empty

    override def contentParsable: Parsable[Torah.Numbered] = new Parsable[Torah.Numbered]:
      override def parser: Parser[Torah.Numbered] = numberedParser
      override def unparser: Unparser[Torah.Numbered] = ???

  object Maftir extends Element[SpanSemiResolved]("maftir"):
    override def contentParsable: Parsable[SpanSemiResolved] = new Parsable[SpanSemiResolved]:
      override def parser: Parser[SpanSemiResolved] = semiResolvedParser
      override def unparser: Unparser[SpanSemiResolved] = ???

  private def numberedParser: Parser[Torah.Numbered] = WithNumber.parse(semiResolvedParser)

  private def semiResolvedParser: Parser[SpanSemiResolved] = SpanParsed.parser.map(_.semiResolve)
