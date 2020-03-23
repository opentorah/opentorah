package org.opentorah.tei

import org.opentorah.entity.EntityReference
import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[EntityReference]
)

object Editor extends Element[Editor](
  elementName = "editor",
  parser = for {
    role <- Attribute("role").optional
    persName <- EntityReference.personParsable.optional
  } yield new Editor(
    role,
    persName
  )
) with ToXml[Editor] {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>
      {value.persName.map(reference => EntityReference.toXml(reference)).orNull}
    </editor>
}
