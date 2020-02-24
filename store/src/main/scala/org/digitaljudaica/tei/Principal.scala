package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Principal(xml: Elem)

object Principal extends DescriptorRaw[Principal]("principal", new Principal(_))
