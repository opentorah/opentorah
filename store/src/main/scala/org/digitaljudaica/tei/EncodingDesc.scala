package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class EncodingDesc(xml: Elem)

object EncodingDesc extends DescriptorRaw[EncodingDesc]("encodingDesc", new EncodingDesc(_))
