package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class SeriesStmt(xml: Elem)

object SeriesStmt extends DescriptorRaw[SeriesStmt]("seriesStmt", new SeriesStmt(_))
