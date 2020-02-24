package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Body(xml: Elem)

object Body extends DescriptorRaw[Body]("body", new Body(_))
