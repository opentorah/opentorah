package org.podval.calendar.generate.tanach

import scala.xml.Elem
import Tanach.{ChumashBook, ChumashBookStructure, NachBook, NachBookStructure}
import SpanParser.{NumberedSpan, SpanParsed, spanAttributes, parseNumberedSpan, parseSpan, addImplied1,
  checkNumber, dropNumbers, setImpliedTo}

object TanachParser {
  def parse: (
    Map[ChumashBook, ChumashBookStructure],
    Map[NachBook, NachBookStructure]
  ) = {
    val baseUrl = XML.baseUrl
    val books: Seq[(Names, Seq[Elem])] =
      XML.checkMeta(XML.loadResource(XML.childFileURL(baseUrl, "Tanach")).get, "Tanach").map { element =>
        XML.loadSubresource(element, "book", baseUrl)
      }
    Names.checkDisjoint(books.map(_._1))

    val chumash = Tanach.chumash.zip(books.take(Tanach.chumash.length)).map {
      case (book: ChumashBook, (names: Names, elements: Seq[Elem])) =>
        require(names.has(book.name))
        book -> parseChumashBook(book, names, elements)
    }

    val nach = Tanach.nach.zip(books.drop(Tanach.chumash.length)).map {
      case (book: NachBook, (names: Names, elements: Seq[Elem])) =>
        require(names.has(book.name))
        book -> parseNachBook(book, names, elements)
    }

    (chumash.toMap, nach.toMap)
  }

  private def parseNachBook(
    book: NachBook,
    names: Names,
    elements: Seq[Elem]
  ): NachBookStructure = {
    val (chapterElements, tail) = XML.span(elements, "chapter")
    XML.checkNoMoreElements(tail)
    new NachBookStructure(
      book,
      names,
      chapters = parseChapters(book, chapterElements)
    )
  }

  private def parseChumashBook(
    book: ChumashBook,
    names: Names,
    elements: Seq[Elem]
  ): ChumashBookStructure = {
    val (chapterElements, weekElements) = XML.span(elements, "chapter", "week")
    val chapters: Chapters = parseChapters(book, chapterElements)
    val weeksPreparsed: Seq[WeekPreparsed] = weekElements.map(preparseWeek)
    val weekSpans: Seq[Span] = setImpliedTo(weeksPreparsed.map(_.span), chapters.full, chapters)
    require(weekSpans.length == weeksPreparsed.length)

    val weeksParsed: Seq[WeekParsed] = weeksPreparsed.zip(Parsha.forBook(book)).zip(weekSpans).map {
      case ((week: WeekPreparsed, parsha: Parsha), span: Span) =>
        require(week.names.has(parsha.name))
        parseWeek(week, parsha, span)
    }

    val weeks: Map[Parsha, Parsha.Structure] = weeksParsed.map { week =>
       week.parsha -> processWeek(week, chapters)
    }.toMap

    new ChumashBookStructure(
      book,
      weeksParsed.head.names,
      chapters,
      weeks
    )
  }

  private final case class ChapterParsed(n: Int, length: Int)

  private def parseChapters(book: Tanach.Book, elements: Seq[Elem]): Chapters = {
    val chapters: Seq[ChapterParsed] = elements.map { element =>
      val attributes = XML.openEmpty(element, "chapter", Set("n", "length"))
      ChapterParsed(
        n = XML.doGetIntAttribute(attributes, "n"),
        length = XML.doGetIntAttribute(attributes, "length")
      )
    }

    require(chapters.map(_.n) == (1 to chapters.length), "Wrong chapter numbers.")

    new Chapters(chapters.map(_.length).toArray)
  }

  private final class WeekPreparsed(
    val span: SpanParsed,
    val names: Names,
    val elements: Seq[Elem]
  )

  private def preparseWeek(element: Elem): WeekPreparsed = {
    val (attributes, names, elements) = XML.doParseNames(element, "week", Set("n") ++ spanAttributes)
    new WeekPreparsed(
      span = parseSpan(attributes),
      names = names,
      elements = elements
    )
  }

  private final case class WeekParsed(
    parsha: Parsha,
    names: Names,
    span: Span,
    days: Seq[NumberedSpan],
    daysCustom: Map[String, Seq[NumberedSpan]],
    daysCombined: Seq[NumberedSpan],
    daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    aliyot: Seq[NumberedSpan],
    maftir: SpanParsed
  )

  private def parseWeek(week: WeekPreparsed, parsha: Parsha, span: Span): WeekParsed = {
    val (aliyahElements, dayElements, maftirElements) = XML.span(week.elements,
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
      parsha = parsha,
      week.names,
      span = span,
      days = days,
      daysCustom = daysCustom,
      daysCombined = daysCombined,
      daysCombinedCustom = daysCombinedCustom,
      aliyot = aliyahElements.map(parseAliyah),
      maftir = parseMaftir(maftirElements.head)
    )
  }

  private final class DayParsed(
    val span: NumberedSpan,
    val custom: Option[String],
    val isCombined: Boolean
  )

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day", Set("n", "custom", "combined") ++ spanAttributes)
    new DayParsed(
      span = parseNumberedSpan(attributes),
      custom = attributes.get("custom"),
      isCombined = XML.doGetBooleanAttribute(attributes, "combined")
    )
  }

  private def parseAliyah(element: Elem): NumberedSpan = {
    val attributes = XML.openEmpty(element, "aliyah", Set("n") ++ spanAttributes)
    parseNumberedSpan(attributes)
  }

  private def parseMaftir(element: Elem): SpanParsed = {
    val attributes = XML.openEmpty(element, "maftir", spanAttributes)
    parseSpan(attributes)
  }

  private def processWeek(week: WeekParsed, chapters: Chapters): Parsha.Structure = {
    // TODO process combined; check against Parsha what can be combined (bring the combinations over from where they are)
    val (days: Seq[Span], customDays: Map[String, Seq[Span]]) = processDays(week.days, week.daysCustom, week.span, chapters)

    // TODO if Cohen ends in a custom place, does it affect the end of the 3 aliyah on Mon/Thu?
    val aliyotSpan = Span(week.span.from, week.aliyot.last.span.to.getOrElse(days.head.to))
    val aliyotWithImplied1: Seq[NumberedSpan] = addImplied1(week.aliyot, aliyotSpan, chapters)
    val aliyot: Seq[Span] = setImpliedTo(dropNumbers(checkNumber(aliyotWithImplied1, 3)), aliyotSpan, chapters)

    val maftir: Span = setImpliedTo(
      Seq(week.maftir),
      Span(week.maftir.from, week.maftir.to.getOrElse(week.span.to)),
      chapters
    ).head

    new Parsha.Structure(
      parsha = week.parsha,
      names = week.names,
      span = week.span,
      days = days,
      customDays = customDays,
      maftir = maftir,
      aliyot = aliyot
    )
  }

  private def processDays(
    days: Seq[NumberedSpan],
    daysCustom: Map[String, Seq[NumberedSpan]],
    span: Span,
    chapters: Chapters
  ): (Seq[Span], Map[String, Seq[Span]]) = {
    val daysWithImplied1 = addImplied1(days, span, chapters)

    def complete(different: Seq[NumberedSpan]): Seq[NumberedSpan] = {
      val result: Array[NumberedSpan] = daysWithImplied1.toArray
      different.foreach(span => result(span.n-1) = span)
      result.toSeq
    }

    def process(spans: Seq[NumberedSpan]): Seq[Span] =
      setImpliedTo(dropNumbers(checkNumber(spans, 7)), span, chapters)

    (
      process(daysWithImplied1),
      daysCustom.mapValues { spans: Seq[NumberedSpan] => process(complete(spans)) }
    )
  }
}
