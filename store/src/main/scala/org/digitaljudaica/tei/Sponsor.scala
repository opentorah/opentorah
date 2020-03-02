package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class Sponsor(xml: Seq[Node]) extends RawXml(xml)

object Sponsor extends DescriptorRawXml[Sponsor]("sponsor", new Sponsor(_))
