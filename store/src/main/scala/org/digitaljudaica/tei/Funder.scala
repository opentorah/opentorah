package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Funder(xml: Seq[Node]) extends RawXml(xml)

object Funder extends DescriptorRawXml[Funder]("funder", new Funder(_))
