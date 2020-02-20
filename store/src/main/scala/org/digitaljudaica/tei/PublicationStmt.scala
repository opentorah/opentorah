package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class PublicationStmt(xml: Elem)

object PublicationStmt
  extends DescriptorRaw[PublicationStmt]("publicationStmt", xml => new PublicationStmt(xml))
