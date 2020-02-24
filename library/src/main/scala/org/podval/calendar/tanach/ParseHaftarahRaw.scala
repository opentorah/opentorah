package org.podval.calendar.tanach

import org.digitaljudaica.metadata.WithNames
import org.digitaljudaica.xml.{ContentType, From, Parser, Xml}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    Parser.parseDo(From.xml(element).parse(ContentType.Elements,
      Xml.withName("haftarah", Haftarah.parser(full))
    )).map(_.from(this), full)
}
