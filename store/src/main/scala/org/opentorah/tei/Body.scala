package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class Body(xml: Seq[Node]) extends RawXml(xml)

object Body extends RawXml.Descriptor[Body]("body", new Body(_))
