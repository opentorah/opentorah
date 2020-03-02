package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Abstract(xml: Seq[Node]) extends RawXml(xml)

object Abstract extends DescriptorRawXml[Abstract]("abstract", new Abstract(_))
