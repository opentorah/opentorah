package org.opentorah.schedule.tanach

import org.opentorah.metadata.WithNames
import org.opentorah.texts.tanach.Torah
import scala.xml.Elem

trait ParseTorah extends ParseTorahRaw { self: WithNames =>
  final lazy val torah: Torah = parseTorah(torahElement)

  protected def torahElement: Elem
}
