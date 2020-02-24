package org.digitaljudaica.tei

import org.digitaljudaica.xml.{Descriptor, Xml}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Elem]
)

object Editor extends Descriptor[Editor](
  elementName = "editor",
  contentParser = for {
    role <- Xml.attribute.optional("role")
    persName <- Xml.optional("persName")
  } yield new Editor(
    role,
    persName
  )
)
