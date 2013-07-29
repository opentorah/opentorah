package org.podval.judaica.structure

import scala.xml.Elem


abstract class NonDivElement(final val label: String) extends Element {

  final override def recognizes(elem: Elem): Boolean = (elem.label == label)
}
