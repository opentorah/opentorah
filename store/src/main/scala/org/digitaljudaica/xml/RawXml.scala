package org.digitaljudaica.xml

import scala.xml.Node

abstract class RawXml(xml: Seq[Node]) {
  def getXml: Seq[Node] = xml
}
