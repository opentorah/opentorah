package org.digitaljudaica.archive.collector

import org.digitaljudaica.xml.PaigesPrettyPrinter

import scala.xml.Elem

// TODO move back into store after fine-tuning
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
