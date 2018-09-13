package org.podval.calendar.generate.tanach

import org.podval.calendar.generate.tanach.SpanParser.{NumberedSpan, SpanParsed, parseSpan, parseNumberedSpan,
  addImplied1, setImpliedTo, checkNumber, dropNumbers}
import org.podval.calendar.metadata.MetadataParser.{MetadataPreparsed, bind}
import org.podval.calendar.metadata.{HasNames, Names, XML}

import scala.xml.Elem

object ParshaMetadataParser {

  def parse(metadata: Seq[MetadataPreparsed], parshiot: Seq[Parsha], chapters: Chapters): Seq[(Parsha, Parsha.Structure)] = {
    val weeksPreparsed: Seq[Preparsed] = metadata.map(preparseWeek)
    val weekSpans: Seq[Span] = setImpliedTo(weeksPreparsed.map(_.span), chapters.full, chapters)
    require(weekSpans.length == weeksPreparsed.length)
    val weeksParsed: Seq[Parsed] = weeksPreparsed.zip(weekSpans).map { case (week, span) =>
      week.parse(span, chapters)
    }
    val weeksCombined: Seq[Combined] = combine(weeksParsed)
    val weeksBound: Seq[(Parsha, Combined)] = bind(parshiot, weeksCombined)
    weeksBound.map { case (parsha: Parsha, week: Combined) => parsha -> week.squash(parsha, chapters) }
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

      def split(days: Seq[DayParsed]): (Seq[NumberedSpan], Map[String, Seq[NumberedSpan]]) = {
        def toNumberedSpan(days: Seq[DayParsed]): Seq[NumberedSpan] = days.map(_.span)
        val (default, custom) = days.partition(_.custom.isEmpty)
        (toNumberedSpan(default), custom.groupBy(_.custom.get).mapValues(toNumberedSpan))
      }

      val daysParsed = dayElements.map(parseDay)
      val (normal: Seq[DayParsed], combined: Seq[DayParsed]) = daysParsed.partition(!_.isCombined)
      val (days, daysCustom) = split(normal)
      val (daysCombined, daysCombinedCustom) = split(combined)

      val (daysProcessed: Seq[Span], daysCustomProcessed: Map[String, Seq[Span]]) =
        processDays(days, daysCustom, span, chapters)

      val aliyot: Seq[NumberedSpan] = aliyahElements.map(parseAliyah)
      // TODO if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
      // TODO if the parshiot combine, does it affect those small aliyot?
      val aliyotSpan: Span = Span(span.from, aliyot.last.span.to.getOrElse(daysProcessed.head.to))
      val aliyotWithImplied1: Seq[NumberedSpan] = addImplied1(aliyot, aliyotSpan, chapters)
      val aliyotResult: Seq[Span] = setImpliedTo(dropNumbers(
        checkNumber(aliyotWithImplied1, 3)), aliyotSpan, chapters)

      val maftir: SpanParsed = parseMaftir(maftirElements.head)
      val maftirResult: Span = setImpliedTo(
        Seq(maftir),
        Span(maftir.from, maftir.to.getOrElse(span.to)),
        chapters
      ).head

      Parsed(
        names,
        span = span,
        days = daysProcessed,
        daysCustom = daysCustomProcessed,
        daysCombined = daysCombined,
        daysCombinedCustom = daysCombinedCustom,
        aliyot = aliyotResult,
        maftir = maftirResult
      )
    }
  }

  private def combine(weeks: Seq[Parsed]): Seq[Combined] = weeks match {
    case week1 :: week2 :: tail =>
      week1.combine(week2.daysCombined, week2.daysCombinedCustom, week2.span) +: combine(week2 +: tail)
    case week :: Nil =>
      Seq(week.combine(Seq.empty, Map.empty, Span(Verse(1, 1), Verse(1, 1)))) // The Span will never be used!
    case Nil =>
      Nil
  }

  private final case class Parsed(
    names: Names,
    span: Span,
    days: Seq[Span],
    daysCustom: Map[String, Seq[Span]],
    daysCombined: Seq[NumberedSpan],
    daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    aliyot: Seq[Span],
    maftir: Span
  ) {
    def combine(
      daysCombinedNext: Seq[NumberedSpan],
      daysCombinedCustomNext: Map[String, Seq[NumberedSpan]],
      spanNext: Span
    ): Combined = new Combined(
      names = names,
      span = span,
      days = days,
      daysCustom = daysCustom,
      daysCombined = daysCombined,
      daysCombinedCustom = daysCombinedCustom,
      spanNext = spanNext,
      daysCombinedNext = daysCombinedNext,
      daysCombinedCustomNext = daysCombinedCustomNext,
      maftir = maftir,
      aliyot = aliyot
    )
  }

  final class Combined(
    val names: Names,
    val span: Span,
    val days: Seq[Span],
    val daysCustom: Map[String, Seq[Span]],
    val daysCombined: Seq[NumberedSpan],
    val daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    val spanNext: Span,
    val daysCombinedNext: Seq[NumberedSpan],
    val daysCombinedCustomNext: Map[String, Seq[NumberedSpan]],
    val maftir: Span,
    val aliyot: Seq[Span]
  ) extends HasNames {
    def squash(parsha: Parsha, chapters: Chapters): Parsha.Structure = {
      val (daysCombinedResult: Seq[Span], daysCombinedCustomResult: Map[String, Seq[Span]]) =
        if (!parsha.combines) (Seq.empty, Map.empty) else {
          // TODO Use defaults from days?
          val daysCombinedResult: Seq[NumberedSpan] = daysCombined ++ daysCombinedNext
          val daysCombinedCustomResult: Map[String, Seq[NumberedSpan]] = {
            val result = daysCombinedCustom.map { case (custom, days) =>
              (custom, days ++ daysCombinedCustomNext.getOrElse(custom, Seq.empty)) }
            daysCombinedCustomNext ++ result
          }
          processDays(daysCombinedResult, daysCombinedCustomResult, chapters.merge(span, spanNext), chapters)
        }

      new Parsha.Structure(
        parsha = parsha,
        names = names,
        span = span,
        days = days,
        daysCustom = daysCustom,
        daysCombined = daysCombinedResult,
        daysCombinedCustom = daysCombinedCustomResult,
        maftir = maftir,
        aliyot = aliyot
      )
    }
  }

  private final class DayParsed(
    val span: NumberedSpan,
    val custom: Option[String],
    val isCombined: Boolean
  )

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day")
    val result = new DayParsed(
      span = parseNumberedSpan(attributes),
      custom = attributes.get("custom"),
      isCombined = attributes.doGetBoolean("combined")
    )
    attributes.close()
    result
  }

  private def parseAliyah(element: Elem): NumberedSpan = {
    val attributes = XML.openEmpty(element, "aliyah")
    val result = parseNumberedSpan(attributes)
    attributes.close()
    result
  }

  private def parseMaftir(element: Elem): SpanParsed = {
    val attributes = XML.openEmpty(element, "maftir")
    val result = parseSpan(attributes)
    attributes.close()
    result
  }

  private def processDays(
    days: Seq[NumberedSpan],
    daysCustom: Map[String, Seq[NumberedSpan]],
    span: Span,
    chapters: Chapters
  ): (Seq[Span], Map[String, Seq[Span]]) = {
    val withImplied1 = addImplied1(days, span, chapters)

    def process(spans: Seq[NumberedSpan]): Seq[Span] =
      setImpliedTo(dropNumbers(checkNumber(spans, 7)), span, chapters)

    (
      process(withImplied1),
      daysCustom.mapValues { spans: Seq[NumberedSpan] => process(overlaySpans(withImplied1, spans)) }
    )
  }

  private def overlaySpans(base: Seq[NumberedSpan], differences: Seq[NumberedSpan]): Seq[NumberedSpan] = {
    val result: Array[NumberedSpan] = base.toArray
    differences.foreach(span => result(span.n-1) = span)
    result.toSeq
  }
}
