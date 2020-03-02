package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class EditionStmt(xml: Elem) extends RawXml(xml)

object EditionStmt extends DescriptorRawXml[EditionStmt]("editionStmt", new EditionStmt(_))
