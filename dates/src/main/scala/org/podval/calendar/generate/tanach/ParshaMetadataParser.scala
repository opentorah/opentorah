package org.podval.calendar.generate.tanach

import org.podval.calendar.generate.tanach.SpanParser.{NumberedSpan, SpanParsed, parseSpan, parseNumberedSpan,
  addImplied1, setImpliedTo, checkNumber, dropNumbers}
import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed
import org.podval.calendar.metadata.{Named, Names, XML}
import Parsha.Parsha

import scala.xml.Elem

object ParshaMetadataParser {

  def parse(metadata: Seq[MetadataPreparsed], chapters: Chapters): Seq[Combined] = {
    val weeksPreparsed: Seq[Preparsed] = metadata.map(preparseWeek)
    val weekSpans: Seq[Span] = setImpliedTo(weeksPreparsed.map(_.span), chapters.full, chapters)
    require(weekSpans.length == weeksPreparsed.length)
    val weeksParsed: Seq[Parsed] = weeksPreparsed.zip(weekSpans).map { case (week, span) => week.parse(span, chapters) }
    combine(weeksParsed)
  }

  private def preparseWeek(metadata: MetadataPreparsed): Preparsed = {
    val result = new Preparsed(
      span = parseSpan(metadata.attributes),
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

      def byCustom(days: Seq[DayParsed]): Custom.Of[Seq[NumberedSpan]] = {
        def toNumberedSpan(days: Seq[DayParsed]): Seq[NumberedSpan] = days.map(_.span)
        val (common, custom) = days.partition(_.custom.isEmpty)
        val result: Custom.Of[Seq[DayParsed]] = custom.groupBy(_.custom.get)
        require(!result.contains(Custom.Common))
        (result + (Custom.Common -> common)).mapValues(toNumberedSpan)
      }

      val (days: Seq[DayParsed], daysCombined: Seq[DayParsed]) = dayElements.map(parseDay).partition(!_.isCombined)
      val daysResult: Custom.Of[Seq[Span]] = processDays(byCustom(days), span, chapters)

      val aliyot: Seq[NumberedSpan] = aliyahElements.map(element => parseNumberedSpan(element, "aliyah"))
      // TODO if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
      // TODO if the parshiot combine, does it affect those small aliyot?
      val aliyotSpan: Span = Span(span.from, aliyot.last.span.to.getOrElse(daysResult(Custom.Common).head.to))
      val aliyotWithImplied1: Seq[NumberedSpan] = addImplied1(aliyot, aliyotSpan, chapters)
      val aliyotResult: Seq[Span] = setImpliedTo(dropNumbers(
        checkNumber(aliyotWithImplied1, 3)), aliyotSpan, chapters)

      val maftir: SpanParsed = parseSpan(maftirElements.head, "maftir")
      val maftirResult: Span = setImpliedTo(
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
    days: Custom.Of[Seq[Span]],
    daysCombined: Custom.Of[Seq[NumberedSpan]],
    aliyot: Seq[Span],
    maftir: Span
  ) {
    def combine(
      daysCombinedNext: Custom.Of[Seq[NumberedSpan]],
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
    val days: Custom.Of[Seq[Span]],
    val daysCombined: Custom.Of[Seq[NumberedSpan]],
    val spanNext: Span,
    val daysCombinedNext: Custom.Of[Seq[NumberedSpan]],
    val maftir: Span,
    val aliyot: Seq[Span]
  ) extends Named.HasNames {
    def squash(parsha: Parsha, chapters: Chapters): Parsha.Structure = {
      def combine: Custom.Of[Seq[Span]] = {
        // TODO Use defaults from days?
        val result = daysCombinedNext ++ daysCombined.map { case (custom, days) =>
          (custom, days ++ daysCombinedNext.getOrElse(custom, Seq.empty))
        }
        processDays(result, chapters.merge(span, spanNext), chapters)
      }

      new Parsha.Structure(
        parsha = parsha,
        names = names,
        span = span,
        days = days,
        daysCombined = if (!parsha.combines) Map.empty else combine,
        maftir = maftir,
        aliyot = aliyot
      )
    }
  }

  private final class DayParsed(
    val span: NumberedSpan,
    val custom: Option[Custom.Custom],
    val isCombined: Boolean
  )

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day")
    val result = new DayParsed(
      span = parseNumberedSpan(attributes),
      // TODO allow *lists* of customs here as in Haftarah?
      custom = attributes.get("custom").map(Custom.getForName),
      isCombined = attributes.doGetBoolean("combined")
    )
    attributes.close()
    result
  }

  private def processDays(
    days: Custom.Of[Seq[NumberedSpan]],
    span: Span,
    chapters: Chapters
  ): Custom.Of[Seq[Span]] = {
    val withImplied1 = addImplied1(days(Custom.Common), span, chapters)

    days.mapValues { spans: Seq[NumberedSpan] =>
      val overlayedSpans = SpanParser.overlaySpans(withImplied1, spans)
      setImpliedTo(dropNumbers(checkNumber(overlayedSpans, 7)), span, chapters)
    }
  }
}
