package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Abstract(xml: Elem)

object Abstract extends DescriptorRaw[Abstract]("abstract", new Abstract(_))
