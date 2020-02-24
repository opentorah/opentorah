package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class Sponsor(xml: Elem)

object Sponsor extends DescriptorRaw[Sponsor]("sponsor", new Sponsor(_))
