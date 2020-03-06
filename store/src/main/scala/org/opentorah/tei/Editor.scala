package org.opentorah.tei

import org.opentorah.xml.{Descriptor, ElementRaw, Xml}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Elem]
)

object Editor extends Descriptor[Editor](
  elementName = "editor",
  contentParser = for {
    role <- Xml.attribute.optional("role")
    persName <- ElementRaw("persName").optional
  } yield new Editor(
    role,
    persName
  )
) {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>{value.persName.orNull}</editor>
}
