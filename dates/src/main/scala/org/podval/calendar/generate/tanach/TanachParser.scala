package org.podval.calendar.generate.tanach

import scala.xml.Elem
import XML.{doGetIntAttribute, getIntAttribute}

object TanachParser {
  private val (
    chumash: Map[Tanach.ChumashBook, Tanach.ChumashBookStructure],
    nach: Map[Tanach.NachBook, Tanach.NachBookStructure]
  ) = {
    val baseUrl = XML.baseUrl
    val books: Seq[(Names, Seq[Elem])] =
      XML.checkMeta(XML.loadResource(XML.childFileURL(baseUrl, "Tanach")).get, "Tanach").map { element =>
        XML.loadSubresource(element, "book", baseUrl)
      }
    Names.checkDisjoint(books.map(_._1))

    val chumash = Tanach.chumash.zip(books.take(Tanach.chumash.length)).map {
      case (book: Tanach.ChumashBook, (names: Names, elements: Seq[Elem])) =>
        require(names.has(book.name))
        book -> parseChumashBook(book, names, elements)
    }

    val nach = Tanach.nach.zip(books.drop(Tanach.chumash.length)).map {
      case (book: Tanach.NachBook, (names: Names, elements: Seq[Elem])) =>
        require(names.has(book.name))
        book -> parseNachBook(book, names, elements)
    }

    (chumash.toMap, nach.toMap)
  }

  def forBook(book: Tanach.ChumashBook): Tanach.ChumashBookStructure = chumash(book)

  def forBook(book: Tanach.NachBook): Tanach.NachBookStructure = nach(book)

  def forBookName(name: String): Option[Tanach.Book] =
    chumash.find(_._2.names.has(name)).map(_._1).orElse(nach.find(_._2.names.has(name)).map(_._1))

  private val parsha2structure: Map[Parsha, Parsha.Structure] = chumash.flatMap {
    case (book: Tanach.ChumashBook, structure: Tanach.ChumashBookStructure) =>
      Parsha.forBook(book).zip(structure.weeks)
  }

  def forParsha(parsha: Parsha): Parsha.Structure = parsha2structure(parsha)

  def forParshaName(name: String): Option[Parsha] =
    parsha2structure.find(_._2.names.has(name)).map(_._1)

  private def parseChumashBook(
    book: Tanach.ChumashBook,
    names: Names,
    elements: Seq[Elem]
  ): Tanach.ChumashBookStructure = {
    val (chapterElements, weekElements) = XML.span(elements, "chapter", "week")
    require(weekElements.nonEmpty, "No weeks.")
    val chapters = parseChapters(chapterElements)
    val weeksParsed = weekElements.map(parseWeek)

    // Validate 'from' for each week.
    weeksParsed.foreach(week => validate(week.fromChapter, week.fromVerse, chapters))
    val firstWeek = weeksParsed.head
    require((firstWeek.fromChapter == 1) && (firstWeek.fromVerse == 1),
      s"First week doesn't start at 1:1: ${firstWeek.names}")

    def setImpliedTo(week: WeekParsed, toChapterImplied: Int, toVerseImplied: Int): WeekParsed = {
      validateImpliedTo(week, toChapterImplied, toVerseImplied)
      week.copy(toChapter = Some(toChapterImplied), toVerse = Some(toVerseImplied))
    }

    // Set implied toChapter/toVerse on weeks.
    val weeksWithTo =
      weeksParsed.zip(weeksParsed.tail).map { case (week, nextWeek) =>
        val (toChapterImplied, toVerseImplied) = prev(nextWeek.fromChapter, nextWeek.fromVerse, chapters)
        setImpliedTo(week, toChapterImplied, toVerseImplied)
      } :+ {
        val (lastToChapter, lastToVerse) = last(chapters)
        setImpliedTo(weeksParsed.last, lastToChapter, lastToVerse)
      }

    new Tanach.ChumashBookStructure(
      firstWeek.names,
      chapters,
      weeksWithTo.map(week => processWeek(week, chapters))
    )
  }

  private def parseNachBook(
    book: Tanach.NachBook,
    names: Names,
    elements: Seq[Elem]
  ): Tanach.NachBookStructure = {
    val (chapterElements, tail) = XML.span(elements, "chapter")
    XML.checkNoMoreElements(tail)
    new Tanach.NachBookStructure(
      names,
      chapters = parseChapters(chapterElements)
    )
  }

  private final case class ChapterParsed(n: Int, length: Int)

  private def parseChapters(elements: Seq[Elem]): Array[Int] = {
    val chapters: Seq[ChapterParsed] = elements.map { element =>
      val attributes = XML.openEmpty(element, "chapter", Set("n", "length"))
      ChapterParsed(
        n = doGetIntAttribute(attributes, "n"),
        length = doGetIntAttribute(attributes, "length")
      )
    }

    require(chapters.map(_.n) == (1 to chapters.length), "Wrong chapter numbers.")

    chapters.map(_.length).toArray
  }

  private trait Fragment {
    def fromChapter: Int
    def fromVerse: Int
    def toChapter: Option[Int]
    def toVerse: Option[Int]
  }

  private def validateImpliedTo(fragment: Fragment, toChapterImplied: Int, toVerseImplied: Int): Unit = {
    require(fragment.toChapter.isEmpty || fragment.toChapter.contains(toChapterImplied), "Wrong explicit 'toChapter'")
    require(fragment.toVerse.isEmpty || fragment.toVerse.contains(toVerseImplied), "Wrong explicit 'toVerse'")
  }

  private final case class WeekParsed(
    names: Names,
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int],
    days: Seq[DayParsed],
    aliyot: Seq[AliyahParsed],
    maftir: MaftirParsed
  ) extends Fragment

  private def parseWeek(element: Elem): WeekParsed = {
    val (attributes, names, elements) = XML.doParseNames(element, "week", Set("n", "fromChapter", "fromVerse"))
    val (aliyahElements, dayElements, maftirElements) = XML.span(elements, "aliyah", "day", "maftir")
    // TODO introduce convenience methods:
    require(maftirElements.nonEmpty, s"No 'maftir' in $names.")
    require(maftirElements.size <= 1, "Spurious 'maftir'.")

    WeekParsed(
      names,
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = None,
      toVerse = None,
      days = dayElements.map(parseDay),
      aliyot = aliyahElements.map(parseAliyah),
      maftir = parseMaftir(maftirElements.head)
    )
  }

  // TODO validate aliyah against chapters.
  private def processWeek(week: WeekParsed, chapters: Array[Int]): Parsha.Structure = {
    val fromChapter = week.fromChapter
    val fromVerse = week.fromVerse
    val toChapter = week.toChapter.get
    val toVerse = week.toVerse.get
    // TODO and pack the results
    val days: Array[Parsha.Day] = processDays(
      week.days,
      chapters,
      fromChapter: Int,
      fromVerse: Int,
      toChapter,
      toVerse
    )
    new Parsha.Structure(
      names = week.names,
      fromChapter = fromChapter,
      fromVerse = fromVerse,
      toChapter = toChapter,
      toVerse = toVerse,
      days = days,
      maftir = processMaftir(week.maftir, chapters, toChapter, toVerse)
    )
  }

  private final case class AliyahParsed(
    n: Int,
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int]
  ) extends Fragment

  private def parseAliyah(element: Elem): AliyahParsed = {
    val attributes = XML.openEmpty(element, "aliyah",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse"))
    AliyahParsed(
      n = doGetIntAttribute(attributes, "n"),
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse")
    )
  }

  private final case class DayParsed(
    n: Int,
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int],
    custom: Option[String],
    isCombined: Boolean
  ) extends Fragment

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.openEmpty(element, "day",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse", "custom", "combined"))
    DayParsed(
      n = doGetIntAttribute(attributes, "n"),
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse"),
      custom = attributes.get("custom"),
      isCombined = XML.doGetBooleanAttribute(attributes, "combined")
    )
  }

  private def processDays(
    days: Seq[DayParsed],
    chapters: Array[Int],
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Int,
    toVerse: Int
  ): Array[Parsha.Day] = {
    val (defaultDays, nonDefaultDays) = days.partition(day => !day.isCombined && day.custom.isEmpty)

    val implied1: Seq[DayParsed] = if (defaultDays.head.n == 1) Seq.empty else Seq(DayParsed(
      n = 1,
      fromChapter = fromChapter,
      fromVerse = fromVerse,
      toChapter = None,
      toVerse = None,
      custom = None,
      isCombined = false
    ))

    val last = defaultDays.last
    val implied7 = if (last.n == 7) Seq.empty[DayParsed] else {
      val (nextChapter, nextVerse) = next(last.toChapter.get, last.toVerse.get, chapters)
      Seq(DayParsed(
        n = 7,
        fromChapter = nextChapter,
        fromVerse = nextVerse,
        toChapter = Some(toChapter),
        toVerse = Some(toVerse),
        custom = None,
        isCombined = false
      ))
    }

    processDaysSequence(
      days = implied1 ++ defaultDays ++ implied7,
      fromChapter,
      fromVerse,
      toChapter,
      toVerse,
      chapters
    )

    // TODO process add customs and combined
    // TODO check against Parsha what can be combined
  }

  private def processDaysSequence(
    days: Seq[DayParsed],
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Int,
    toVerse: Int,
    chapters: Array[Int]
  ): Array[Parsha.Day] = {
    require(days.map(_.n) == (1 to 7), "Wrong day numbers.")

    def setImpliedTo(day: DayParsed, toChapterImplied: Int, toVerseImplied: Int): DayParsed = {
      validateImpliedTo(day, toChapterImplied, toVerseImplied)
      day.copy(toChapter = Some(toChapterImplied), toVerse = Some(toVerseImplied))
    }

    // Set implied toChapter/toVerse on days.
    val daysWithTo = days.zip(days.tail).map { case (day, nextDay) =>
      val (toChapterImplied, toVerseImplied) = prev(nextDay.fromChapter, nextDay.fromVerse, chapters)
      setImpliedTo(day, toChapterImplied, toVerseImplied)
    } :+ {
      setImpliedTo(days.last, toChapter, toVerse)
    }

    // First day has to start correctly
    val first = daysWithTo.head
    require((first.fromChapter == fromChapter) && (first.fromVerse == fromVerse))

    daysWithTo.map { day =>
      validate(day.fromChapter, day.fromVerse, chapters)
      validate(day.toChapter.get, day.toVerse.get, chapters)

      new Parsha.Day(
        fromChapter = day.fromChapter,
        fromVerse = day.fromVerse,
        toChapter = day.toChapter.get,
        toVerse = day.toVerse.get
      )
    }.toArray
  }

  private final case class MaftirParsed(
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int]
  ) extends Fragment

  private def parseMaftir(element: Elem): MaftirParsed = {
    val attributes = XML.openEmpty(element, "maftir",
      Set("fromChapter", "fromVerse"))
    MaftirParsed(
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = None,
      toVerse = None
    )
  }

  private def processMaftir(maftir: MaftirParsed, chapters: Array[Int], toChapter: Int, toVerse: Int): Parsha.Maftir = {
    validate(maftir.fromChapter, maftir.fromVerse, chapters)
    validateImpliedTo(maftir, toChapter, toVerse)
    new Parsha.Maftir(
      fromChapter = maftir.fromChapter,
      fromVerse = maftir.fromVerse,
      toChapter = toChapter,
      toVerse = toVerse
    )
  }

  def validate(chapter: Int, verse: Int, chapters: Array[Int]): Unit = {
    require(chapter > 0, "Non-positive chapter.")
    require(verse > 0, "Non-positive verse.")
    require(chapter <= chapters.length, "Chapter out of range")
    require(verse <= chapters(chapter-1), s"Verse $verse out of chapter #$chapter of length ${chapters(chapter-1)}")
  }

  def prev(chapter: Int, verse: Int, chapters: Array[Int]): (Int, Int) = {
    validate(chapter, verse, chapters)
    if (verse > 1) (chapter, verse-1)
    else if (chapter > 1) (chapter-1, chapters(chapter-2))
    else throw new IllegalArgumentException("No chapters before the first one.")
  }

  def next(chapter: Int, verse: Int, chapters: Array[Int]): (Int, Int) = {
    validate(chapter, verse, chapters)
    if (verse < chapters(chapter)) (chapter, verse+1)
    else if (chapter < chapters.length) (chapter+1, 1)
    else throw new IllegalArgumentException("No chapters after the last one.")
  }

  def last(chapters: Array[Int]): (Int, Int) = (chapters.length, chapters(chapters.length-1))
}
