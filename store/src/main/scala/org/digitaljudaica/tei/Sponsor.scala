package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class Sponsor(xml: Elem) extends RawXml(xml)

object Sponsor extends DescriptorRawXml[Sponsor]("sponsor", new Sponsor(_))
