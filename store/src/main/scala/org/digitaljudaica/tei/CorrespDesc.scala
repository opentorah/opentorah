package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class CorrespDesc(xml: Elem) extends RawXml(xml)

object CorrespDesc extends DescriptorRawXml[CorrespDesc]("correspDesc", new CorrespDesc(_))
