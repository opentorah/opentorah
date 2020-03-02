package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class NotesStmt(xml: Seq[Node]) extends RawXml(xml)

object NotesStmt extends DescriptorRawXml[NotesStmt]("notesStmt", new NotesStmt(_))
