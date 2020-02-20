package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Creation(xml: Elem)

object Creation extends DescriptorRaw[Creation](
  elementName = "creation",
  create = xml => new Creation(xml)
)
