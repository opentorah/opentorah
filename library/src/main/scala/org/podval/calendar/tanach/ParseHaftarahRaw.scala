package org.podval.calendar.tanach

import org.digitaljudaica.metadata.WithNames
import org.digitaljudaica.xml.{From, Xml}
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    From.xml(element).parseDo[Haftarah.Customs](Xml.withName("haftarah", Haftarah.parser(full)))
      .map(_.from(this), full)
}
