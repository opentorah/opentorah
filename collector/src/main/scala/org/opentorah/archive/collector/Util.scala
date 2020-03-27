package org.opentorah.archive.collector

import java.io.File
import org.opentorah.util.Files
import org.opentorah.xml.PaigesPrettyPrinter
import scala.xml.Elem

object Util {

  def writeXml(
    file: File,
    elem: Elem
  ): Unit = Files.write(
    file,
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + renderTei(elem) + "\n"
  )

  def renderTei(elem: Elem): String = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  ).render(elem)

  def renderHtml(elem: Elem): String = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
  ).render(elem)
}
