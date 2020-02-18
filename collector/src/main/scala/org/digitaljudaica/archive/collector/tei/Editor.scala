package org.digitaljudaica.archive.collector.tei

import org.digitaljudaica.xml.Ops._
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Elem]
)

object Editor {

  // TODO rework with From/Parser
  def apply(xml: Elem): Editor = new Editor(
    role = xml.attributeOption("role"),
    persName = xml.optionalChild("persName")
  )
}
