package org.digitaljudaica.xml

import scala.xml.Elem

abstract class RawXml(xml: Elem) {
  def getXml: Elem = xml
}
