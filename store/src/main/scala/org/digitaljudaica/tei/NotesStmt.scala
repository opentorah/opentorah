package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class NotesStmt(xml: Elem) extends RawXml(xml)

object NotesStmt extends DescriptorRawXml[NotesStmt]("notesStmt", new NotesStmt(_))
