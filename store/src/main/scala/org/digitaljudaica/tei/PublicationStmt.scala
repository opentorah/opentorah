package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

// TODO elaborate according to TEI Guidelines.
final case class PublicationStmt(xml: Elem) extends RawXml(xml)

object PublicationStmt extends DescriptorRawXml[PublicationStmt]("publicationStmt", new PublicationStmt(_))
