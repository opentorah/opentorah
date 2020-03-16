package org.opentorah.judaica.tanach

import org.opentorah.metadata.{Metadata, Names, WithNumber}
import org.opentorah.util.Collections
import org.opentorah.xml.{Attribute, ContentType, Element, Parser}
import scala.xml.Elem

final class ParshaMetadata(
  val parsha: Parsha,
  val names: Names,
  val span: Span,
  val days: Torah.Customs,
  val daysCombined: Option[Torah.Customs],
  val aliyot: Torah,
  val maftir: Torah.Maftir
)

object ParshaMetadata {

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
    ): ParshaMetadata = {

      val days = daysResolved(parshaSpan)
      val aliyot = aliyotResolved(parshaSpan, days)
      val maftir = maftirResolved(parshaSpan)

      new ParshaMetadata(
        parsha,
        names,
        parshaSpan,
        days,
        daysCombined,
        aliyot,
        maftir
      )
    }

    private def daysResolved(parshaSpan: Span): Torah.Customs =
      Torah.processDays(parsha.book, days, parshaSpan)

    private def aliyotResolved(parshaSpan: Span, days: Torah.Customs): Torah = {
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

  private final case class DayParsed(
    span: Torah.Numbered,
    custom: Set[Custom],
    isCombined: Boolean
  )

  def parser(book: Tanach.ChumashBook): Parser[Parsed] = for {
    names <- Names.parser
    span <- semiResolvedParser
    aliyot <- AliyahParsable.all
    daysParsed <- DayParsable.all
    maftir <- MaftirParsable.required
    parsha = Metadata.find[Parsha, Names](book.parshiot, names)
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

  private object AliyahParsable extends Element[Torah.Numbered](
    elementName = "aliyah",
    ContentType.Empty,
    parser = numberedParser
  ) {
    override def toXml(value: Torah.Numbered): Elem = ??? // TODO
  }

  private object DayParsable extends Element[DayParsed](
    elementName = "day",
    parser = dayParser
  ) {
    override def toXml(value: DayParsed): Elem = ??? // TODO
  }

  private object MaftirParsable extends Element[SpanSemiResolved](
    elementName = "maftir",
    parser = semiResolvedParser
  ) {
    override def toXml(value: SpanSemiResolved): Elem = ??? // TODO
  }

  private def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[Torah.Numbered]] =
    Collections.mapValues(days.groupBy(_.custom))(days => days.map(_.span))

  private def dayParser: Parser[DayParsed] = for {
    span <- numberedParser
    custom <- Attribute("custom").optional.map(_.fold[Set[Custom]](Set(Custom.Common))(Custom.parse))
    isCombined <- Attribute("combined").boolean.optional.map(_.getOrElse(false))
  } yield DayParsed(span, custom, isCombined)

  private def numberedParser: Parser[Torah.Numbered] = WithNumber.parse(semiResolvedParser)

  private def semiResolvedParser: Parser[SpanSemiResolved] = SpanParsed.parser.map(_.semiResolve)
}
