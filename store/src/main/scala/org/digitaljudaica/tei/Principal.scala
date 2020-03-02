package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Principal(xml: Seq[Node]) extends RawXml(xml)

object Principal extends DescriptorRawXml[Principal]("principal", new Principal(_))
