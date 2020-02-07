package org.podval.calendar.tanach

import org.digitaljudaica.metadata.{WithNames, Xml}

import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs = {
    val (attributes, elements) = Xml.open(element, "haftarah")
    val result = Haftarah.parse(attributes, elements, full = full)
    result.map(_.from(this), full = full)
  }
}
