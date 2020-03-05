package org.digitaljudaica.tei

import org.digitaljudaica.xml.{Descriptor, Raw, Xml}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Elem]
)

object Editor extends Descriptor[Editor](
  elementName = "editor",
  contentParser = for {
    role <- Xml.attribute.optional("role")
    persName <- Raw("persName").optional
  } yield new Editor(
    role,
    persName
  )
) {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>{value.persName.orNull}</editor>
}
