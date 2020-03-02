package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class EditionStmt(xml: Seq[Node]) extends RawXml(xml)

object EditionStmt extends DescriptorRawXml[EditionStmt]("editionStmt", new EditionStmt(_))
