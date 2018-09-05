package org.podval.calendar.generate.tanach

import scala.xml.Elem
import XML.{doGetIntAttribute, getIntAttribute}
import Tanach.{ChumashBook, ChumashBookStructure, NachBook, NachBookStructure}

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
    val weeksParsed: Seq[WeekParsed] = weekElements.map(parseWeek)

    val weekSpans: Seq[Span] = setImpliedTo(weeksParsed.map(_.span), chapters.full, chapters)
    require(weekSpans.length == weeksParsed.length)

    val weeks: Map[Parsha, Parsha.Structure] = Parsha.forBook(book).zip(weeksParsed).zip(weekSpans).map {
      case ((parsha: Parsha, week: WeekParsed), span: Span) =>
        require(week.names.has(parsha.name))
        parsha -> processWeek(parsha, week, span, chapters)
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
        n = doGetIntAttribute(attributes, "n"),
        length = doGetIntAttribute(attributes, "length")
      )
    }

    require(chapters.map(_.n) == (1 to chapters.length), "Wrong chapter numbers.")

    new Chapters(chapters.map(_.length).toArray)
  }

  private final case class WeekParsed(
    names: Names,
    span: SpanParsed,
    days: Seq[DayParsed],
    aliyot: Seq[NumberedSpan],
    maftir: SpanParsed
  )

  private def parseWeek(element: Elem): WeekParsed = {
    val (attributes, names, elements) = XML.doParseNames(element, "week", Set("n", "fromChapter", "fromVerse"))
    val (aliyahElements, dayElements, maftirElements) = XML.span(elements, "aliyah", "day", "maftir")
    require(maftirElements.length == 1)

    WeekParsed(
      names,
      span = parseSpan(attributes),
      days = dayElements.map(parseDay),
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

  // TODO reuse Span etc. in Haftorot parsing.

  private val spanAttributes = Set("fromChapter", "fromVerse", "toChapter", "toVerse")

  private final class NumberedSpan(val n: Int, val span: SpanParsed)

  private def parseNumberedSpan(attributes: Map[String, String]): NumberedSpan = new NumberedSpan(
    n = doGetIntAttribute(attributes, "n"),
    span = parseSpan(attributes)
  )

  private final class SpanParsed(val from: Verse, val to: Option[Verse]) {
    def setTo(value: Verse): Span = {
      require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
      Span(from, value)
    }
  }

  private def parseSpan(attributes: Map[String, String]): SpanParsed = {
    val from = parseFrom(attributes)
    val toChapter = getIntAttribute(attributes, "toChapter")
    val toVerse = getIntAttribute(attributes, "toVerse")
    val to = if (toVerse.isEmpty) {
      require(toChapter.isEmpty)
      None
    } else {
      Some(Verse(toChapter.getOrElse(from.chapter), toVerse.get))
    }
    new SpanParsed(from, to)
  }

  private def parseFrom(attributes: Map[String, String]): Verse = Verse(
    doGetIntAttribute(attributes, "fromChapter"),
    doGetIntAttribute(attributes, "fromVerse")
  )

  private def processWeek(parsha: Parsha, week: WeekParsed, span: Span, chapters: Chapters): Parsha.Structure = {
    val days: Seq[Span] = processDays(week.days, span, chapters)

    val aliyot: Seq[Span] = processSpanSequence(
      week.aliyot,
      3,
      Span(span.from, week.aliyot.last.span.to.getOrElse(days.head.to)),
      chapters
    )

    val maftir: Span = setImpliedTo(
      Seq(week.maftir),
      Span(week.maftir.from, week.maftir.to.getOrElse(span.to)),
      chapters
    ).head

    new Parsha.Structure(
      parsha = parsha,
      names = week.names,
      span = span,
      days = days,
      maftir = maftir,
      aliyot = aliyot
    )
  }

  private def processDays(days: Seq[DayParsed], span: Span, chapters: Chapters): Seq[Span] = {
    def process(spans: Seq[NumberedSpan]): Seq[Span] = processSpanSequence(spans, number = 7, span, chapters)

    // TODO process customs and combined
    // TODO check against Parsha what can be combined (bring the combinations over from where they are)
    val defaultDays: Seq[NumberedSpan] = days
      .filter(day => !day.isCombined && day.custom.isEmpty)
      .map(_.span)

    def complete(different: Seq[NumberedSpan]): Seq[NumberedSpan] = ???

    val customDays: Map[String, Seq[NumberedSpan]] = days
      .filter(day => !day.isCombined && day.custom.nonEmpty)
      .groupBy(_.custom.get)
      .mapValues { days => complete(days.map(_.span)) }

    val result = process(defaultDays)

    result
  }

  private def addImplied1(
    spans: Seq[NumberedSpan],
    number: Int,
    span: Span,
    chapters: Chapters
  ): Seq[NumberedSpan] = {
    val first = spans.head
    val implied: Seq[NumberedSpan] = if (first.n == 1) Seq.empty else Seq(new NumberedSpan(1, new SpanParsed(
      span.from,
      Some(chapters.prev(first.span.from).get)
    )))

    val result: Seq[NumberedSpan] = implied ++ spans
    require(result.map(_.n) == (1 to number), "Wrong number of spans.")

    result
  }

  private def processSpanSequence(
    spans: Seq[NumberedSpan],
    number: Int,
    span: Span,
    chapters: Chapters
  ): Seq[Span] = processNumberedSpanSequence(
    addImplied1(spans, number, span, chapters),
    span,
    chapters
  )

  private def processNumberedSpanSequence(
    spans: Seq[NumberedSpan],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = setImpliedTo(
    spans.map(_.span),
    span,
    chapters
  )

  private def setImpliedTo(
    spans: Seq[SpanParsed],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (span, to) => span.setTo(to) }
    require(chapters.cover(result, span))

    result
  }
}
