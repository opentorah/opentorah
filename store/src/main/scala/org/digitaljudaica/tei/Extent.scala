package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Extent(xml: Elem) extends RawXml(xml)

object Extent extends DescriptorRawXml[Extent]("extent", new Extent(_))
