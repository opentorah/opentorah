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
        week.parse(parsha, span)
    }

    val weeksProcessed: Seq[WeekProcessed] = weeksParsed.map(_.process(chapters))
    val weeks: Seq[Parsha.Structure] = processCombined(weeksProcessed, chapters)

    new ChumashBookStructure(
      book,
      weeksParsed.head.names,
      chapters,
      weeks.map(week => week.parsha -> week).toMap
    )
  }

  private def processCombined(weeks: Seq[WeekProcessed], chapters: Chapters): Seq[Parsha.Structure] = weeks match {
    case week1 :: week2 :: tail =>
      if (!week1.combines) {
        require(week1.isDaysCombinedEmpty)
        week1.postProcess() +: processCombined(week2 +: tail, chapters)
      } else {
        require(!week2.combines)
        require(!week1.isDaysCombinedEmpty)
        require(!week2.isDaysCombinedEmpty)
        val (daysCombined, daysCombinedCustom) = processCombined(week1, week2, chapters)
        week1.postProcess(daysCombined, daysCombinedCustom) +: week2.postProcess() +: processCombined(tail, chapters)
      }

    case week :: Nil =>
      require(!week.combines)
      Seq(week.postProcess())

    case Nil => Nil
  }

  private def preparseWeek(element: Elem): WeekPreparsed = {
    val (attributes, names, elements) = XML.doParseNames(element, "week", Set("n") ++ spanAttributes)
    new WeekPreparsed(
      span = parseSpan(attributes),
      names = names,
      elements = elements
    )
  }

  private final class WeekPreparsed(
    val span: SpanParsed,
    val names: Names,
    val elements: Seq[Elem]
  ) {
    def parse(parsha: Parsha, span: Span): WeekParsed = {
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
        parsha = parsha,
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
    parsha: Parsha,
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
        parsha = parsha,
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
    val parsha: Parsha,
    val names: Names,
    val span: Span,
    val days: Seq[Span], // length 7 :)
    val daysCustom: Map[String, Seq[Span]],
    val daysCombined: Seq[NumberedSpan],
    val daysCombinedCustom: Map[String, Seq[NumberedSpan]],
    val maftir: Span,
    val aliyot: Seq[Span] // length 3
  ) {
    def combines: Boolean = parsha.combines

    def isDaysCombinedEmpty: Boolean = daysCombined.isEmpty && daysCombinedCustom.isEmpty

    def postProcess(): Parsha.Structure = postProcess(Seq.empty, Map.empty)

    def postProcess(daysCombined: Seq[Span], daysCombinedCustom: Map[String, Seq[Span]]): Parsha.Structure = {
      new Parsha.Structure(
        parsha = parsha,
        names = names,
        span = span,
        days = days,
        daysCustom = daysCustom,
        daysCombined = daysCombined,
        daysCombinedCustom = daysCombinedCustom,
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

  private def processCombined(week1: WeekProcessed, week2: WeekProcessed, chapters: Chapters): (Seq[Span], Map[String, Seq[Span]]) = {
    // TODO Use defaults from week1.days?
    val daysCombined: Seq[NumberedSpan] = week1.daysCombined ++ week2.daysCombined
    val daysCombinedCustom: Map[String, Seq[NumberedSpan]] = {
      val result = week1.daysCombinedCustom.map { case (custom, days) =>
        (custom, days ++ week2.daysCombinedCustom.getOrElse(custom, Seq.empty))
      }
      val more = week2.daysCombinedCustom.filterKeys(custom => !result.contains(custom))
      result ++ more
    }
    processDays(daysCombined, daysCombinedCustom, chapters.merge(week1.span, week2.span), chapters)
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
