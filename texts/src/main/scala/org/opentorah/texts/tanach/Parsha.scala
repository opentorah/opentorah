package org.opentorah.texts.tanach


import org.opentorah.util.Collections
import org.podval.metadata.{HasName, HasValues, Names}
import org.podval.store.{By, Stores}
import org.podval.xml.XmlError
import Tanach.Chumash

enum Parsha(val book: Chumash, nameOverride: Option[String] = None) extends
  HasName(nameOverride),
  HasName.Enum,
  Stores[?] derives CanEqual:

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

  override def stores: Seq[By[?]] = Seq(
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
    ): ParshaMetadata =
      val days: Torah.Customs = daysResolved(parshaSpan)
      val aliyot: Torah = aliyotResolved(parshaSpan, days)
      ParshaMetadata(
        parsha,
        names,
        parshaSpan,
        days,
        daysCombined,
        aliyot,
        maftirResolved(parshaSpan)
      )

    private def daysResolved(parshaSpan: Span): Torah.Customs =
      Torah.processDays(parsha.book, days, parshaSpan)

    private def aliyotResolved(parshaSpan: Span, days: Torah.Customs): Torah =
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

  def decode(book: ChumashBook, dto: ParshaWeekDto): Parsed =
    val names: Names = dto.weekNames
    val span: SpanSemiResolved = dto.span
    val aliyot: Seq[Torah.Numbered] = dto.aliyot.map(_.numberedSemi)
    val daysParsed: Seq[DayParsed] = dto.days.map(decodeDay)
    if dto.maftirs.length != 1 then throw XmlError(s"Required maftir, found ${dto.maftirs.length}")
    val maftir: SpanSemiResolved = dto.maftirs.head.span.semiResolve
    val parsha: Parsha = HasName.findByNames(book.parshiot, names)
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

  private def decodeDay(dto: DayDto): DayParsed = DayParsed(
    span = dto.span,
    custom = dto.custom.fold[Set[Custom]](Set(Custom.Common))(Custom.parse),
    isCombined = dto.combined.getOrElse(false)
  )

  private def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[Torah.Numbered]] =
    Collections.mapValues(days.groupBy(_.custom))(days => days.map(_.span))
