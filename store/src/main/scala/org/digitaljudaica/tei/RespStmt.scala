package org.digitaljudaica.tei

import org.digitaljudaica.xml.RawXml
import scala.xml.Node

// TODO contains resp and name
final case class RespStmt(xml: Seq[Node]) extends RawXml(xml)

object RespStmt extends RawXml.Descriptor[RespStmt]("respStmt", new RespStmt(_))
