package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class EditionStmt(xml: Seq[Node]) extends RawXml(xml)

object EditionStmt extends RawXml.Descriptor[EditionStmt]("editionStmt", new EditionStmt(_))
