package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final class Part(documentNames: Seq[String]) {
}

object Part {
  def apply(xml: Elem): Part =
    new Part(documentNames = xml.elemsFilter("document").map(_.text))
}