package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class TextClass(xml: Seq[Node]) extends RawXml(xml)

object TextClass extends DescriptorRawXml[TextClass]("textClass", new TextClass(_))
