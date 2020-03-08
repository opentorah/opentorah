package org.opentorah.archive.collector

import org.opentorah.xml.PaigesPrettyPrinter
import scala.xml.Elem

// TODO unfold into Main
object Print {

  private val prettyPrinter = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  )

  def render(elem: Elem): String = prettyPrinter.render(elem)
}
