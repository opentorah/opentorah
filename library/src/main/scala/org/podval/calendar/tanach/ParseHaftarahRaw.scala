package org.podval.calendar.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.xml.{From, Parser, Xml}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    Parser.parseDo(
      From.xml(element).parse(Xml.withName("haftarah", Haftarah.parser(full)))
    ).map(_.from(this), full)
}
