package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import scala.xml.Elem

trait ParseHaftarah extends ParseHaftarahRaw { self: WithNames =>
  final lazy val haftarah: Haftarah.Customs = parseHaftarah(haftarahElement)

  protected def haftarahElement: Elem
}
