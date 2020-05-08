package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNames
import scala.xml.Elem

trait ParseTorah extends ParseTorahRaw { self: WithNames =>
  final lazy val torah: Torah = parseTorah(torahElement)

  protected def torahElement: Elem
}
