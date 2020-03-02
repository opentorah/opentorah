package org.digitaljudaica.xml

import scala.xml.Elem

object Print {

  private val width: Int = 120

  private val indent: Int = 2

  private val prettyPrinter = new PaigesPrettyPrinter(width, indent)

  def render(elem: Elem): String = prettyPrinter.render(elem)
}
