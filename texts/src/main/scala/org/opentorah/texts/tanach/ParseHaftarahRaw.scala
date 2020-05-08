package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import scala.xml.Elem

trait ParseHaftarahRaw { self: WithNames =>
  protected final def parseHaftarah(element: Elem, full: Boolean = true): Haftarah.Customs =
    Haftarah.parse(element, full).map(_.from(this), full)
}
