package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Extent(xml: Elem)

object Extent extends DescriptorRaw[Extent]("extent", new Extent(_))
