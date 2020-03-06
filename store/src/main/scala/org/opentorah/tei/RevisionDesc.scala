package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class RevisionDesc(xml: Seq[Node]) extends RawXml(xml)

object RevisionDesc extends RawXml.Descriptor[RevisionDesc]("revisionDesc", new RevisionDesc(_))
