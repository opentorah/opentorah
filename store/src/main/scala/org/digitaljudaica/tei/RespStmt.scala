package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

// TODO contains resp and name
final case class RespStmt(xml: Elem)

object RespStmt extends DescriptorRaw[RespStmt]("respStmt", new RespStmt(_))
