package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{Element, From, Parser}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    ParseHaftarahRaw.parseHaftarah(element, full).map(_.from(this), full)
}

object ParseHaftarahRaw {

  def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs = Parser.parseDo(
    new Element[Haftarah.Customs](
      elementName = "haftarah",
      parser = Haftarah.parser(full)
    ).parse(From.xml("Haftarah", element)))
}
