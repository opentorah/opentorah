package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{WithNames, Xml}
import org.podval.judaica.tanach.{SpanParsed, SpanSemiResolved, Torah, WithNumber}

import scala.xml.Elem

trait ParseTorahRaw { self: WithNames =>

  // TODO switch to Parser[A]
  final def parseTorah(element: Elem): Torah = Xml.runA[Torah](element, "torah", parser)

  private val parser: Xml.Parser[Torah] = for {
    bookSpanParsed <- Torah.parseSpanNg
    bookSpan = bookSpanParsed.resolve
    spans <- Xml.elements("aliyah", parseNumbered(bookSpan.span.from.chapter))
    result = Torah.parseAliyot(bookSpan, spans, number = None)
  } yield result.fromWithNumbers(this)

  private def parseNumbered(fromChapter: Int): Xml.Parser[Torah.Numbered] = for {
    result <- WithNumber.parseNg(spanParser(fromChapter))
  } yield result

  private def spanParser(fromChapter: Int): Xml.Parser[SpanSemiResolved] = for {
    spanParsed <- SpanParsed.parser
  } yield spanParsed.defaultFromChapter(fromChapter).semiResolve
}
