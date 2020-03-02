package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class TextClass(xml: Elem) extends RawXml(xml)

object TextClass extends DescriptorRawXml[TextClass]("textClass", new TextClass(_))
