package org.opentorah.texts.tanach

import org.opentorah.metadata.{Metadata, Named, NamedCompanion, Names, WithNumber}
import org.opentorah.util.Collections
import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, Parsable, Parser}

sealed trait Parsha extends Named {
  def book: Chumash

  private def metadata: Parsha.ParshaMetadata = book.metadata.forParsha(this)

  final override def names: Names = metadata.names

  final def span: Span = metadata.span

  final def days: Torah.Customs = metadata.days

  final def daysCombined: Option[Torah.Customs] = metadata.daysCombined

  final def getDaysCombined: Torah.Customs = {
    require(this.combines)
    daysCombined.get
  }

  final def aliyot: Torah = metadata.aliyot

  final def maftir: Torah.Maftir = metadata.maftir

  final def combines: Boolean = Parsha.combinable.contains(this)

  final def haftarah: Haftarah.Customs = Haftarah.haftarah(this).map(_.from(this))
}

object Parsha extends NamedCompanion {

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
  ) {
    def resolve(
      parshaSpan: Span,
      daysCombined: Option[Torah.Customs]
    ): Parser[ParshaMetadata] = {
      for {
        days <- daysResolved(parshaSpan)
        aliyot <- aliyotResolved(parshaSpan, days)
        maftir = maftirResolved(parshaSpan)
      } yield new ParshaMetadata(
        parsha,
        names,
        parshaSpan,
        days,
        daysCombined,
        aliyot,
        maftir
      )
    }

    private def daysResolved(parshaSpan: Span): Parser[Torah.Customs] =
      Torah.processDays(parsha.book, days, parshaSpan)

    private def aliyotResolved(parshaSpan: Span, days: Torah.Customs): Parser[Torah] = {
      val bookSpan = Torah.inBook(parsha.book,
        Span(
          parshaSpan.from,
          aliyot.last.what.to.getOrElse(days.common.spans.head.span.to)
        )
      )
      Torah.parseAliyot(bookSpan, aliyot, number = Some(3))
    }

    private def maftirResolved(parshaSpan: Span): Torah.Maftir = {
      val span = Span(maftir.from, maftir.to.getOrElse(parshaSpan.to))

      Torah.inBook(parsha.book,
        SpanSemiResolved.setImpliedTo(
          Seq(maftir),
          span,
          parsha.book.chapters
        ).head
      )
    }
  }

  final class WeekParsable(book: Chumash)
    extends Element[Parsed]("week")
  {
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed] {
      override def parser: Parser[Parsed] = Parsha.parser(book)
      override def unparser: Unparser[Parsed] = ???
    }
  }

  private def parser(book: Chumash): Parser[Parsed] = for {
    names <- Names.withoutDefaultNameParsable()
    span <- semiResolvedParser
    aliyot <- Aliyah.seq()
    daysParsed <- DayParsed.seq()
    maftir <- Maftir.required()
    parsha <- Metadata.find[Parsha](book.parshiot, names)
  } yield {
    val (days: Seq[DayParsed], daysCombined: Seq[DayParsed]) = daysParsed.partition(!_.isCombined)
    new Parsed(
      parsha,
      names,
      span,
      days = byCustom(days),
      daysCombined = byCustom(daysCombined),
      aliyot,
      maftir
    )
  }

  private final case class DayParsed(
    span: Torah.Numbered,
    custom: Set[Custom],
    isCombined: Boolean
  )

  private object DayParsed extends Element[DayParsed]("day") {
    override def contentParsable: Parsable[DayParsed] = new Parsable[DayParsed] {
      override def parser: Parser[DayParsed] = for {
        span <- numberedParser
        custom <- Attribute("custom").optional().map(_.fold[Set[Custom]](Set(Custom.Common))(Custom.parse))
        isCombined <- new Attribute.BooleanAttribute("combined").optional().map(_.getOrElse(false))
      } yield DayParsed(span, custom, isCombined)

      override def unparser: Unparser[DayParsed] = ???
    }
  }

  private def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[Torah.Numbered]] =
    Collections.mapValues(days.groupBy(_.custom))(days => days.map(_.span))

  object Aliyah extends Element[Torah.Numbered]("aliyah") {
    override def contentType: ContentType = ContentType.Empty

    override def contentParsable: Parsable[Torah.Numbered] = new Parsable[Torah.Numbered] {
      override def parser: Parser[Torah.Numbered] = numberedParser
      override def unparser: Unparser[Torah.Numbered] = ???
    }
  }

  object Maftir extends Element[SpanSemiResolved]("maftir") {
    override def contentParsable: Parsable[SpanSemiResolved] = new Parsable[SpanSemiResolved] {
      override def parser: Parser[SpanSemiResolved] = semiResolvedParser
      override def unparser: Unparser[SpanSemiResolved] = ???
    }
  }

  private def numberedParser: Parser[Torah.Numbered] = WithNumber.parse(semiResolvedParser)

  private def semiResolvedParser: Parser[SpanSemiResolved] = SpanParsed.parser.map(_.semiResolve)

  override type Key = Parsha

  trait GenesisParsha extends Parsha { final override def book: Chumash = Chumash.Genesis }

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

  trait ExodusParsha extends Parsha { final override def book: Chumash = Chumash.Exodus }

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

  trait LeviticusParsha extends Parsha { final override def book: Chumash = Chumash.Leviticus }

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

  trait NumbersParsha extends Parsha { final override def book: Chumash = Chumash.Numbers }

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

  trait DeutoronomyParsha extends Parsha { final override def book: Chumash = Chumash.Deuteronomy }

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

  final override val values: Seq[Parsha] = genesis ++ exodus ++ leviticus ++ numbers ++ deuteronomy

  // Rules of combining; affect the WeeklyReading.
  final val combinableFromBereishisToVayikra: Seq[Parsha] = Seq(Vayakhel)
  // Reversing the priorities here currently affects only non-leap regular years with Rosh
  // Hashanah on Thursday (and Pesach on Shabbat).
  final val combinableFromVayikraToBemidbar: Seq[Parsha] = Seq(Tazria, Acharei, Behar)
  final val combinableFromBemidbarToVa_eschanan: Seq[Parsha] = Seq(Mattos, Chukas)
  final val combinableFromVa_eschanan: Seq[Parsha] = Seq(Nitzavim)

  final val combinable: Set[Parsha] = (combinableFromBereishisToVayikra ++ combinableFromVayikraToBemidbar ++
    combinableFromBemidbarToVa_eschanan ++ combinableFromVa_eschanan).toSet
}
