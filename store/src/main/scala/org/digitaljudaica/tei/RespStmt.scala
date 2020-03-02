package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

// TODO contains resp and name
final case class RespStmt(xml: Seq[Node]) extends RawXml(xml)

object RespStmt extends DescriptorRawXml[RespStmt]("respStmt", new RespStmt(_))
