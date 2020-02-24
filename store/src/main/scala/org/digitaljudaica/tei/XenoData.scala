package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class XenoData(xml: Elem)

object XenoData extends DescriptorRaw[XenoData]("xenoData", new XenoData(_))
