package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Funder(xml: Elem) extends RawXml(xml)

object Funder extends DescriptorRawXml[Funder]("funder", new Funder(_))
