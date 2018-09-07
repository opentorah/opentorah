package org.podval.calendar.generate.tanach

import java.net.URL

import scala.xml.Elem
import Tanach.{ChumashBook, ChumashBookStructure, NachBook, NachBookStructure}
import SpanParser.{NumberedSpan, SpanParsed, addImplied1, checkNumber, dropNumbers, parseNumberedSpan, parseSpan, setImpliedTo}
import org.podval.calendar.metadata.MetadataParser.{MetadataPreparsed, bind}
import org.podval.calendar.metadata.{Metadata, MetadataParser, Names, XML}

object TanachParser {
  def parse(obj: AnyRef): (
    Map[ChumashBook, ChumashBookStructure],
    Map[NachBook, NachBookStructure]
  ) = {
    val (className, url) = MetadataParser.getUrl(obj)
    val books: Seq[MetadataPreparsed] = MetadataParser.loadMetadataResource(url, className, "book")

    (
      bind(Tanach.chumash, books.take(Tanach.chumash.length)).map(parseChumashBook(url, _)).toMap,
      bind(Tanach.nach, books.drop(Tanach.chumash.length)).map(parseNachBook).toMap
    )
  }

  private def parseNachBook(arg: (NachBook, MetadataPreparsed)): (NachBook, NachBookStructure) = {
    val (book: NachBook, metadata: MetadataPreparsed) = arg
    metadata.attributes.close()
    val (chapterElements, tail) = XML.span(metadata.elements, "chapter")
    XML.checkNoMoreElements(tail)
    val result = new NachBookStructure(
      book,
      metadata.names,
      chapters = parseChapters(book, chapterElements)
    )
    book -> result
  }

  private final case class ChapterParsed(n: Int, length: Int)

  private def parseChapters(book: Tanach.Book[_], elements: Seq[Elem]): Chapters = {
    val chapters: Seq[ChapterParsed] = elements.map { element =>
      val attributes = XML.openEmpty(element, "chapter" )
      val result = ChapterParsed(
        n = attributes.doGetInt("n"),
        length = attributes.doGetInt("length")
      )
      attributes.close()
      result
    }

    require(chapters.map(_.n) == (1 to chapters.length), "Wrong chapter numbers.")

    new Chapters(chapters.map(_.length).toArray)
  }

  private def parseChumashBook(
    url: URL,
    arg: (ChumashBook, MetadataPreparsed)
  ): (ChumashBook,  ChumashBookStructure) = {
    val (book: ChumashBook, metadata: MetadataPreparsed) = arg
    metadata.attributes.close()
    // TODO handle names from metadata
    val (chapterElements, weekElements) = XML.span(metadata.elements, "chapter", "week")
    val chapters: Chapters = parseChapters(book, chapterElements)

    val weeksMetadata: Seq[MetadataPreparsed] = MetadataParser.loadMetadata(url, weekElements, "week")
    val weeksPreparsed: Seq[WeekPreparsed] = weeksMetadata.map(preparseWeek)
    val weekSpans: Seq[Span] = setImpliedTo(weeksPreparsed.map(_.span), chapters.full, chapters)
    require(weekSpans.length == weeksPreparsed.length)
    val weeksParsed: Seq[WeekParsed] = weeksPreparsed.zip(weekSpans).map { case (week, span) => week.parse(span) }

    val weeksProcessed: Seq[WeekProcessed] = weeksParsed.map(_.process(chapters))
    val weeksCombined: Seq[WeekCombined] = combine(weeksProcessed)
    val weeksBound: Seq[(Parsha, WeekCombined)] = bind(Parsha.forBook(book), weeksCombined)
    val weeks: Seq[(Parsha, Parsha.Structure)] = weeksBound.map { case (parsha: Parsha, week: WeekCombined) =>
        parsha -> week.squash(parsha, chapters)
    }

    val result = new ChumashBookStructure(
      book,
      weeksParsed.head.names,
      chapters,
      weeks.toMap
    )
    book -> result
  }

  private def combine(weeks: Seq[WeekProcessed]): Seq[WeekCombined] = weeks match {
    case week1 :: week2 :: tail =>
      week1.combine(week2.daysCombined, week2.daysCombinedCustom, week2.span) +: combine(week2 +: tail)
    case week :: Nil =>
      Seq(week.combine(Seq.empty, Map.empty, Span(Verse(1, 1), Verse(1, 1))))
    case Nil =>
      Nil
  }

  private def preparseWeek(metadata: MetadataPreparsed): WeekPreparsed = {
    val result = new WeekPreparsed(
      span = parseSpan(metadata.attributes),
      names = metadata.names,
      elements = metadata.elements
    )
    metadata.attributes.close()
    result
  }

  private final class WeekPreparsed(
    val span: SpanParsed,
    val names: Names,
    val elements: Seq[Elem]
  ) {
    def parse(span: Span): WeekParsed = {
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

      WeekParsed(
        names,
        span = span,
        days = days,
        daysCustom = daysCustom,
        daysCombined = daysCombined,
        daysCombinedCustom = daysCombinedCustom,
        aliyot = aliyahElements.map(parseAliyah),
        maftir = parseMaftir(maftirElements.head)
      )
    }
  }

  private final case class WeekParsed(
    names: Names,
    span: Span,
    days: Seq[NumberedSpan],
    daysCustom: Map[String, Seq[NumberedSpan]],
    daysCombined: Seq[NumberedSpan],
    daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    aliyot: Seq[NumberedSpan],
    maftir: SpanParsed
  ) {
    def process(chapters: Chapters): WeekProcessed = {
      // TODO process combined; check against Parsha what can be combined (bring the combinations over from where they are)
      val (daysProcessed: Seq[Span], daysCustomProcessed: Map[String, Seq[Span]]) =
        processDays(days, daysCustom, span, chapters)

      // TODO if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
      // TODO if the parshiot combine, does it affect those small aliyot?
      val aliyotSpan = Span(span.from, aliyot.last.span.to.getOrElse(daysProcessed.head.to))
      val aliyotWithImplied1: Seq[NumberedSpan] = addImplied1(aliyot, aliyotSpan, chapters)
      val aliyotProcessed: Seq[Span] = setImpliedTo(dropNumbers(checkNumber(aliyotWithImplied1, 3)), aliyotSpan, chapters)

      val maftirProcessed: Span = setImpliedTo(
        Seq(maftir),
        Span(maftir.from, maftir.to.getOrElse(span.to)),
        chapters
      ).head

      new WeekProcessed(
        names = names,
        span = span,
        days = daysProcessed,
        daysCustom = daysCustomProcessed,
        daysCombined = daysCombined,
        daysCombinedCustom = daysCombinedCustom,
        maftir = maftirProcessed,
        aliyot = aliyotProcessed
      )
    }
  }

  final class WeekProcessed(
    val names: Names,
    val span: Span,
    val days: Seq[Span], // length 7 :)
    val daysCustom: Map[String, Seq[Span]],
    val daysCombined: Seq[NumberedSpan],
    val daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    val maftir: Span,
    val aliyot: Seq[Span] // length 3
  ) {
    def combine(
      daysCombinedNext: Seq[NumberedSpan],
      daysCombinedCustomNext: Map[String, Seq[NumberedSpan]],
      spanNext: Span
    ): WeekCombined = new WeekCombined(
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

  final class WeekCombined(
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
  ) extends Metadata {
    def squash(parsha: Parsha, chapters: Chapters): Parsha.Structure = {
      val (daysCombinedResult: Seq[Span], daysCombinedCustomResult: Map[String, Seq[Span]]) = {
        if (!parsha.combines) (Seq.empty, Map.empty) else {
          // TODO Use defaults from days?
          val daysCombinedResult: Seq[NumberedSpan] = daysCombined ++ daysCombinedNext
          val daysCombinedCustomResult: Map[String, Seq[NumberedSpan]] = {
            val result = daysCombinedCustom.map { case (custom, days) =>
              (custom, days ++ daysCombinedCustomNext.getOrElse(custom, Seq.empty))
            }
            daysCombinedCustomNext ++ result
          }
          processDays(daysCombinedResult, daysCombinedCustomResult, chapters.merge(span, spanNext), chapters)
        }
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
