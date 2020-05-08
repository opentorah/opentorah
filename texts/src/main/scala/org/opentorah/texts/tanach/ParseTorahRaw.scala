package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{From, Parser}
import scala.xml.Elem

trait ParseTorahRaw { self: WithNames =>

  final def parseTorah(element: Elem): Torah =
    Parser.parseDo(Torah.torahParsable.parse(From.xml("Torah", element)))
      .fromWithNumbers(this)
}
