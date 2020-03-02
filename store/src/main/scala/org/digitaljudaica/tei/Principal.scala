package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Principal(xml: Elem) extends RawXml(xml)

object Principal extends DescriptorRawXml[Principal]("principal", new Principal(_))
