package org.podval.calendar.tanach

import org.podval.judaica.metadata.{WithNames, XML}
import org.podval.judaica.tanach.Torah
import org.podval.judaica.tanach.Torah.Maftir

import scala.xml.Elem

trait ParseMaftir { self: WithNames =>

  final lazy val maftir: Maftir = parseMaftir(maftirElement)

  protected def maftirElement: Elem

  private def parseMaftir(element: Elem): Torah.Maftir = {
    val attributes = XML.openEmpty(element, "maftir")
    val result = Torah.parseSpan(attributes).resolve
    result.from(this)
  }
}
