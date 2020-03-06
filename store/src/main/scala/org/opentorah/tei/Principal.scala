package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class Principal(xml: Seq[Node]) extends RawXml(xml)

object Principal extends RawXml.Descriptor[Principal]("principal", new Principal(_))
