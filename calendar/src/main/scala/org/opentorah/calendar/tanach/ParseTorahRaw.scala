package org.opentorah.calendar.tanach

import org.opentorah.metadata.{WithNames, WithNumber}
import org.opentorah.xml.{Element, From, Parser}
import org.opentorah.judaica.tanach.{SpanParsed, Torah}
import scala.xml.Elem

trait ParseTorahRaw { self: WithNames =>

  final def parseTorah(element: Elem): Torah =
    Parser.parseDo(TorahParsable.parse(From.xml("Torah", element)))

  private object TorahParsable extends Element[Torah](
    elementName = "torah",
    parser = for {
      bookSpan <- Torah.spanParser.map(_.resolve)
      spans <- new Element[Torah.Numbered](
        elementName = "aliyah",
        parser = WithNumber.parse(SpanParsed.parser.map(_.defaultFromChapter(bookSpan.span.from.chapter).semiResolve))
      ).all
    } yield Torah.parseAliyot(bookSpan, spans, number = None).fromWithNumbers(this)
  )
}
