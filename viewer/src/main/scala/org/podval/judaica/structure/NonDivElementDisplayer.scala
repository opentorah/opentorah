package org.podval.judaica.structure

import scala.xml.Elem


abstract class NonDivElementDisplayer(final val label: String) extends ElementDisplayer {

  final override def recognizes(elem: Elem): Boolean = (elem.label == label)
}
