package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class Publisher(xml: Seq[Node]) extends RawXml(xml)

object Publisher extends RawXml.Descriptor[Publisher]("publisher", new Publisher(_))
