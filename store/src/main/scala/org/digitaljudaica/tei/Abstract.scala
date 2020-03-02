package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Abstract(xml: Elem) extends RawXml(xml)

object Abstract extends DescriptorRawXml[Abstract]("abstract", new Abstract(_))
