package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Body(xml: Seq[Node]) extends RawXml(xml)

object Body extends DescriptorRawXml[Body]("body", new Body(_))
