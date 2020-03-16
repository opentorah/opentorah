package org.opentorah.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    Parser.parseDo(
      From.xml(element).parse(haftarahParsable(full))
    ).map(_.from(this), full)

  private def haftarahParsable(full: Boolean): Element[Haftarah.Customs] = new Element[Haftarah.Customs](
    elementName = "haftarah",
    parser = Haftarah.parser(full)
  ) {
    override def toXml(value: Haftarah.Customs): Elem = ??? // TODO
  }
}
