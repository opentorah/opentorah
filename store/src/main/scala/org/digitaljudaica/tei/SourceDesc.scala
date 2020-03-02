package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class SourceDesc(xml: Elem) extends RawXml(xml)

object SourceDesc extends DescriptorRawXml[SourceDesc]("sourceDesc", new SourceDesc(_))
