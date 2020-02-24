package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class EditionStmt(xml: Elem)

object EditionStmt extends DescriptorRaw[EditionStmt]("editionStmt", new EditionStmt(_))
