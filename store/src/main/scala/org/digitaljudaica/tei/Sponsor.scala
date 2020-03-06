package org.digitaljudaica.tei

import org.digitaljudaica.xml.RawXml
import scala.xml.Node

final case class Sponsor(xml: Seq[Node]) extends RawXml(xml)

object Sponsor extends RawXml.Descriptor[Sponsor]("sponsor", new Sponsor(_))
