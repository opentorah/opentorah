package org.opentorah.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    Parser.parseDo(
      From.xml(element).parse(Element.withName("haftarah", Haftarah.parser(full)))
    ).map(_.from(this), full)
}
