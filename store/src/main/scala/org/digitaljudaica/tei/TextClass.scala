package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class TextClass(xml: Elem)

object TextClass extends DescriptorRaw[TextClass]("textClass", new TextClass(_))
