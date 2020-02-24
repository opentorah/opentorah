package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Funder(xml: Elem)

object Funder extends DescriptorRaw[Funder]("funder", new Funder(_))
