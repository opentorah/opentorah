package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class SeriesStmt(xml: Elem) extends RawXml(xml)

object SeriesStmt extends DescriptorRawXml[SeriesStmt]("seriesStmt", new SeriesStmt(_))
