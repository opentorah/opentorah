package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class XenoData(xml: Elem) extends RawXml(xml)

object XenoData extends DescriptorRawXml[XenoData]("xenoData", new XenoData(_))
