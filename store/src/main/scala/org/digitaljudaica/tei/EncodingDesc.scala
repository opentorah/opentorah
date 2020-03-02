package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class EncodingDesc(xml: Elem) extends RawXml(xml)

object EncodingDesc extends DescriptorRawXml[EncodingDesc]("encodingDesc", new EncodingDesc(_))
