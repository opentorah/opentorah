package org.opentorah.schedule.tanach

import org.opentorah.metadata.{WithNames, WithNumber}
import org.opentorah.texts.tanach.{SpanParsed, Torah}
import org.opentorah.xml.{Element, From, Parser}
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
