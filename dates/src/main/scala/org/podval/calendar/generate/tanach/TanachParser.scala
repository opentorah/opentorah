package org.podval.calendar.generate.tanach

import scala.xml.Elem
import XML.getIntAttribute

// TODO monadize!
object TanachParser {
  def parse: Seq[TanachBook] = {
    val baseUrl = XML.baseUrl
    val url = XML.childFileURL(baseUrl, "Tanach")
    val children: Seq[Elem] = XML.children(XML.checkMeta(XML.loadResource(url).get, "Tanach"))
    val books: Seq[(Names, Seq[Elem])] = children.map(NamesParser.doParse(_, "book", Set("n")))
    Names.checkDisjoint(books.map(_._1))
    val books1 = books.map { case (names, elements) =>
      if (elements.nonEmpty) (names, elements) else {
        val subresources: Seq[Elem] = names.names.flatMap(name => XML.loadResource(XML.childFileURL(baseUrl, name.name)))
        if (subresources.size > 1) throw new IllegalArgumentException("More than one subresource.")
        if (subresources.isEmpty) (names, elements) else {
          val (newNames, newElements) = NamesParser.parse(subresources.head, "book", Set("n"))
          val mergedNames: Names = newNames.fold(names)(newNames => Names.merge(names, newNames))
          (mergedNames, newElements)
        }
      }
    }
    books1.take(5).map { case (names, elements) => parseChumashBook(names, elements) } ++
    books1.drop(5).map { case (names, elements) => parseNachhBook(names, elements) }
  }

  private def parseChumashBook(names: Names, elements: Seq[Elem]): ChumashBook = {
    val (chaptersParsed: Seq[ChapterParsed], afterChapters: Seq[Elem]) = parseChapters(elements)
    val (weeksElements, afterWeeks) = afterChapters.span(_.label == "week")
    val weeks: Seq[WeekParsed] = weeksElements.map(parseWeek)
    XML.checkNoMoreElements(afterWeeks)
    if (weeks.isEmpty) throw new IllegalArgumentException("No weeks.")
    ChumashBook(chaptersParsed, weeks)
  }

  private def parseNachhBook(names: Names, elements: Seq[Elem]): NachBook = {
    val (chaptersParsed: Seq[ChapterParsed], afterChapters: Seq[Elem]) = parseChapters(elements)
    XML.checkNoMoreElements(afterChapters)
    NachBook(names, chaptersParsed)
  }

  private def parseChapters(elements: Seq[Elem]): (Seq[ChapterParsed], Seq[Elem]) = {
    val (chapterElements, theRest) = elements.span(_.label == "chapter")
    val result: Seq[ChapterParsed] = chapterElements.map(parseChapter)
    (result, theRest)
  }

  private def parseChapter(element: Elem): ChapterParsed = {
    val attributes = XML.checkElement(element, "chapter", Set("n", "length"))
    ChapterParsed(
      n = getIntAttribute(attributes, "n").get,
      length = getIntAttribute(attributes, "length").get
    )
  }

  private def parseWeek(element: Elem): WeekParsed = {
    val (names, afterNames) = NamesParser.doParse(element, "week", Set("n", "fromChapter", "fromVerse"))
    val attributes = XML.getAttributes(element)

    val (aliyahElements, afterAliyot) = afterNames.span(_.label == "aliyah")
    val (dayElements, afterDays) = afterAliyot.span(_.label == "day")
    val (maftirElements, afterMaftir) = afterDays.span(_.label == "maftir")
    if (maftirElements.isEmpty) throw new IllegalArgumentException(s"No 'maftir' in $names.")
    if (maftirElements.size > 1) throw new IllegalArgumentException("Spurious 'maftir'.")
    XML.checkNoMoreElements(afterMaftir)

    WeekParsed(
      names,
      fromChapter = getIntAttribute(attributes, "fromChapter").get,
      fromVerse = getIntAttribute(attributes, "fromVerse").get,
      days = dayElements.map(parseDay),
      aliyot = aliyahElements.map(parseAliyah),
      parseMaftir(maftirElements.head)
    )
  }

  private def parseAliyah(element: Elem): AliyahParsed = {
    val attributes = XML.checkElement(element, "aliyah",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse"))
    AliyahParsed(
      n = getIntAttribute(attributes, "n").get,
      fromChapter = getIntAttribute(attributes, "fromChapter").get,
      fromVerse = getIntAttribute(attributes, "fromVerse").get,
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse")
    )
  }

  private def parseDay(element: Elem): DayParsed = {
    val attributes = XML.checkElement(element, "day",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse", "custom", "combined"))
    DayParsed(
      n = getIntAttribute(attributes, "n").get,
      fromChapter = getIntAttribute(attributes, "fromChapter").get,
      fromVerse = getIntAttribute(attributes, "fromVerse").get,
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse")
    )
  }

  private def parseMaftir(element: Elem): Maftir = {
    val attributes = XML.checkElement(element, "maftir", Set("fromChapter", "fromVerse"))
    Maftir(
      fromChapter = getIntAttribute(attributes, "fromChapter").get,
      fromVerse = getIntAttribute(attributes, "fromVerse").get
    )
  }

  def main(args: Array[String]): Unit = {
    val result = parse
    val z = 0
  }
}
