package org.podval.judaica.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{Metadata, Names, WithNumber}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{Attribute, Element, Parser}

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
    aliyot <- Element.empty.all("aliyah", numberedParser)
    daysParsed <- Element.all("day", dayParser)
    maftir <- Element.required("maftir", semiResolvedParser)
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

  private def byCustom(days: Seq[DayParsed]): Custom.Sets[Seq[Torah.Numbered]] =
    Collections.mapValues(days.groupBy(_.custom))(days => days.map(_.span))

  private def dayParser: Parser[DayParsed] = for {
    span <- numberedParser
    custom <- Attribute.optional("custom").map(_.fold[Set[Custom]](Set(Custom.Common))(Custom.parse))
    isCombined <- Attribute.optional.boolean("combined").map(_.getOrElse(false))
  } yield DayParsed(span, custom, isCombined)

  private def numberedParser: Parser[Torah.Numbered] = WithNumber.parse(semiResolvedParser)

  private def semiResolvedParser: Parser[SpanSemiResolved] = SpanParsed.parser.map(_.semiResolve)
}
