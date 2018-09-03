package org.podval.calendar.generate.tanach

import scala.xml.Elem
import XML.{getIntAttribute, doGetIntAttribute}

// TODO monadize!
object TanachParser {
  def parse: Seq[TanachBook] = {
    val baseUrl = XML.baseUrl
    val url = XML.childFileURL(baseUrl, "Tanach")
    val children: Seq[Elem] = XML.checkMeta(XML.loadResource(url).get, "Tanach")
    val books: Seq[(Map[String, String], Names, Seq[Elem])] = children.map(NamesParser.doParse(_, "book", Set("n")))
    Names.checkDisjoint(books.map(_._2))
    val books1 = books.map { case (_, names, elements) =>
      if (elements.nonEmpty) (names, elements) else {
        val subresources: Seq[Elem] = names.names.flatMap(name => XML.loadResource(XML.childFileURL(baseUrl, name.name)))
        if (subresources.size > 1) throw new IllegalArgumentException("More than one subresource.")
        if (subresources.isEmpty) (names, elements) else {
          val (_, newNames, newElements) = NamesParser.parse(subresources.head, "book", Set("n"))
          val mergedNames: Names = newNames.fold(names)(newNames => Names.merge(names, newNames))
          (mergedNames, newElements)
        }
      }
    }
    books1.take(5).map { case (names, elements) => parseChumashBook(names, elements) } ++
    books1.drop(5).map { case (names, elements) => parseNachhBook(names, elements) }
  }

  private def parseChumashBook(names: Names, elements: Seq[Elem]): ChumashBook = {
    val (chapterElements, weekElements) = XML.span(elements, "chapter", "week")
    if (weekElements.isEmpty) throw new IllegalArgumentException("No weeks.")
    ChumashBook(
      chaptersParsed = chapterElements.map(parseChapter),
      weeksParsed = weekElements.map(parseWeek)
    )
  }

  private def parseNachhBook(names: Names, elements: Seq[Elem]): NachBook = {
    val (chapterElements, tail) = XML.span(elements, "chapter")
    XML.checkNoMoreElements(tail)
    NachBook(
      names,
      chaptersParsed = chapterElements.map(parseChapter)
    )
  }

  private def parseChapter(element: Elem): ChapterParsed = {
    val attributes = XML.openEmpty(element, "chapter", Set("n", "length"))
    ChapterParsed(
      n = doGetIntAttribute(attributes, "n"),
      length = doGetIntAttribute(attributes, "length")
    )
  }

  private def parseWeek(element: Elem): WeekParsed = {
    val (attributes, names, elements) = NamesParser.doParse(element, "week", Set("n", "fromChapter", "fromVerse"))
    val (aliyahElements, dayElements, maftirElements) = XML.span(elements, "aliyah", "day", "maftir")
    // TODO introduce convenience methods:
    if (maftirElements.isEmpty) throw new IllegalArgumentException(s"No 'maftir' in $names.")
    if (maftirElements.size > 1) throw new IllegalArgumentException("Spurious 'maftir'.")

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

  private def parseAliyah(element: Elem): WeekParsed.Aliyah = {
    val attributes = XML.openEmpty(element, "aliyah",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse"))
    WeekParsed.Aliyah(
      n = doGetIntAttribute(attributes, "n"),
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse")
    )
  }

  private def parseDay(element: Elem): WeekParsed.Day = {
    val attributes = XML.openEmpty(element, "day",
      Set("n", "fromChapter", "fromVerse", "toChapter", "toVerse", "custom", "combined"))
    WeekParsed.Day(
      n = doGetIntAttribute(attributes, "n"),
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = getIntAttribute(attributes, "toChapter"),
      toVerse = getIntAttribute(attributes, "toVerse"),
      custom = attributes.get("custom"),
      isCombined = XML.doGetBooleanAttribute(attributes, "combined")
    )
  }

  private def parseMaftir(element: Elem): WeekParsed.Maftir = {
    val attributes = XML.openEmpty(element, "maftir",
      Set("fromChapter", "fromVerse"))
    WeekParsed.Maftir(
      fromChapter = doGetIntAttribute(attributes, "fromChapter"),
      fromVerse = doGetIntAttribute(attributes, "fromVerse"),
      toChapter = None,
      toVerse = None
    )
  }
}
