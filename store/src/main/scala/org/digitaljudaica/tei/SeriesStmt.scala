package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class SeriesStmt(xml: Seq[Node]) extends RawXml(xml)

object SeriesStmt extends DescriptorRawXml[SeriesStmt]("seriesStmt", new SeriesStmt(_))
