package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Body(xml: Elem) extends RawXml(xml)

object Body extends DescriptorRawXml[Body]("body", new Body(_))
