package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class XenoData(xml: Seq[Node]) extends RawXml(xml)

object XenoData extends DescriptorRawXml[XenoData]("xenoData", new XenoData(_))
