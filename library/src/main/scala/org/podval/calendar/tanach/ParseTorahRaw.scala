package org.podval.calendar.tanach

import org.podval.judaica.metadata.{Attributes, WithNames, XML}
import org.podval.judaica.tanach.{SpanParsed, Torah, WithNumber}

import scala.xml.Elem

trait ParseTorahRaw { self: WithNames =>
  final def parseTorah(element: Elem): Torah = {
    val (attributes: Attributes, elements: Seq[Elem]) = XML.open(element, "torah")
    val bookSpan: Torah.Fragment = Torah.parseSpan(attributes).resolve
    val fromChapter: Int = bookSpan.span.from.chapter
    val spans: Seq[Torah.Numbered] =
      elements.map(element => XML.parseEmpty(element, "aliyah", parseNumbered(fromChapter)))
    val result: Torah = Torah.parseAliyot(bookSpan, spans, number = None)
    result.fromWithNumbers(this)
  }

  private def parseNumbered(fromChapter: Int)(attributes: Attributes): Torah.Numbered = {
    val result = WithNumber.parse(attributes,
      attributes => SpanParsed.parse(attributes).defaultFromChapter(fromChapter).semiResolve)
    require(result.what.to.isEmpty, s"Non-empty to: ${result.what}")
    result
  }
}
