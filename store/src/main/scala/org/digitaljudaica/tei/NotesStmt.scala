package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class NotesStmt(xml: Elem)

object NotesStmt extends DescriptorRaw[NotesStmt]("notesStmt", new NotesStmt(_))
