package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class SeriesStmt(xml: Seq[Node]) extends RawXml(xml)

object SeriesStmt extends RawXml.Descriptor[SeriesStmt]("seriesStmt", new SeriesStmt(_))
