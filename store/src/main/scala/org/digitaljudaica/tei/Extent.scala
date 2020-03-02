package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Extent(xml: Seq[Node]) extends RawXml(xml)

object Extent extends DescriptorRawXml[Extent]("extent", new Extent(_))
