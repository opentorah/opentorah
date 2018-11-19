package org.podval.calendar.schedule.tanach

import org.podval.judaica.metadata.{Attributes, XML}
import org.podval.judaica.metadata.tanach.{Torah, SpanParsed, WithNumber}

import scala.xml.Elem

trait TorahReadings {
  protected final def parseTorahForShabbosAndWeekday(drop1: Int, drop2: Int, element: Elem): (Torah, Torah) =
    parseTorahForShabbosAndWeekday(Set(drop1, drop2), element)

  protected final def parseTorahForShabbosAndWeekday(toDrop: Set[Int], element: Elem): (Torah, Torah) = {
    val shabbosAliyot: Torah = parseTorah(element)
    (shabbosAliyot, drop(shabbosAliyot, toDrop))
  }

  protected final def parseTorah(element: Elem): Torah = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: Torah.Fragment = Torah.parseSpan(attributes).resolve
    val fromChapter: Int = bookSpan.span.from.chapter
    val result: Seq[Torah.Numbered] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
    Torah.parseAliyot(bookSpan, result, number = None)
  }

  private def parseNumbered(fromChapter: Int)(attributes: Attributes): Torah.Numbered = {
    val result = WithNumber.parse(attributes,
      attributes => SpanParsed.parse(attributes).defaultFromChapter(fromChapter).semiResolve)
    require(result.what.to.isEmpty, s"Non-empty to: ${result.what}")
    result
  }

  private final def drop(torah: Torah, toDrop: Set[Int]): Torah = {
    def drop(what: Seq[(Torah.Aliyah, Boolean)]): Seq[Torah.Aliyah] = what match {
      case (a1, d1) :: (a2, d2) :: tail =>
        if (d2) drop((a1+a2, d1) +: tail)
        else a1 +: drop((a2, d2) +: tail)
      case (a1, d1) :: Nil =>
        Seq(a1)
    }

    val withDrop = torah.spans.zipWithIndex.map { case (a, i) => (a, toDrop.contains(i+1)) }
    require(!withDrop.head._2)
    Torah(drop(withDrop))
  }

  protected final def parseMaftir(element: Elem): Torah.Maftir = {
    val attributes = XML.openEmpty(element, "maftir")
    Torah.parseSpan(attributes).resolve
  }
}
