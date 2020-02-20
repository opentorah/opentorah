package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class TitleStmt(xml: Elem)

object TitleStmt
  extends DescriptorRaw[TitleStmt]("titleStmt", xml => new TitleStmt(xml))
