package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class SourceDesc(xml: Elem)

object SourceDesc extends DescriptorRaw[SourceDesc]("sourceDesc", new SourceDesc(_))
