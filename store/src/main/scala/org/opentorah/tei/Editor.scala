package org.opentorah.tei

import org.opentorah.xml.{Attribute, Descriptor, ElementRaw}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Elem]
)

object Editor extends Descriptor[Editor](
  elementName = "editor",
  contentParser = for {
    role <- Attribute("role").optional
    persName <- ElementRaw("persName").optional
  } yield new Editor(
    role,
    persName
  )
) {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>{value.persName.orNull}</editor>
}
