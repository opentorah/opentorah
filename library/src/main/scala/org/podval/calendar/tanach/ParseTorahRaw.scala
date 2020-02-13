package org.podval.calendar.tanach

import cats.implicits._
import org.digitaljudaica.metadata.WithNames
import org.digitaljudaica.xml.{From, Element, Parser}
import org.podval.judaica.tanach.{SpanParsed, Torah, WithNumber}

import scala.xml.Elem

trait ParseTorahRaw { self: WithNames =>

  final def parseTorah(element: Elem): Torah = From.xml(element).parseDo[Torah](Element.withName("torah", parser))

  private val parser: Parser[Torah] = for {
    bookSpan <- Torah.spanParser.map(_.resolve)
    spans <- Element.all("aliyah", WithNumber.parse(
      SpanParsed.parser.map(_.defaultFromChapter(bookSpan.span.from.chapter).semiResolve)
    ))
  } yield Torah.parseAliyot(bookSpan, spans, number = None).fromWithNumbers(this)
}
