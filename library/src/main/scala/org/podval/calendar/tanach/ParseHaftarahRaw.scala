package org.podval.calendar.tanach

import org.podval.judaica.metadata.{WithNames, XML}

import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs = {
    val (attributes, elements) = XML.open(element, "haftarah")
    val result = Haftarah.parse(attributes, elements, full = full)
    result.map(_.from(this), full = full)
  }
}
