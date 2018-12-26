package org.podval.calendar.tanach

import org.podval.judaica.metadata.WithNames
import org.podval.judaica.tanach.Torah

import scala.xml.Elem

trait ParseTorah extends ParseTorahRaw { self: WithNames =>
  final lazy val torah: Torah = parseTorah(torahElement)

  protected def torahElement: Elem
}
