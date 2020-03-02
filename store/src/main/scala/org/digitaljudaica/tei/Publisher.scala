package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Publisher(xml: Seq[Node]) extends RawXml(xml)

object Publisher extends DescriptorRawXml[Publisher]("publisher", new Publisher(_))
