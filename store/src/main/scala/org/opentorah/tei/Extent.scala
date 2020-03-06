package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class Extent(xml: Seq[Node]) extends RawXml(xml)

object Extent extends RawXml.Descriptor[Extent]("extent", new Extent(_))
