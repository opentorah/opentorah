package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

// TODO contains resp and name
final case class RespStmt(xml: Elem) extends RawXml(xml)

object RespStmt extends DescriptorRawXml[RespStmt]("respStmt", new RespStmt(_))
