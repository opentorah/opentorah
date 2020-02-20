package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class CorrespDesc(xml: Elem)

object CorrespDesc extends DescriptorRaw[CorrespDesc]("correspDesc", xml => new CorrespDesc(xml))
