package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class CorrespDesc(xml: Seq[Node]) extends RawXml(xml)

object CorrespDesc extends RawXml.Descriptor[CorrespDesc]("correspDesc", new CorrespDesc(_))
