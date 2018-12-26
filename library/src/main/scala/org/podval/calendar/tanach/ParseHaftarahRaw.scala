package org.podval.calendar.tanach

import org.podval.judaica.metadata.WithNames

import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs = {
    val result: Haftarah.Customs = Haftarah.parse(element, full = full)
    result.map(_.from(this), full = full)
  }
}
