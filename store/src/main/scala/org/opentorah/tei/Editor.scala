package org.opentorah.tei

import org.opentorah.reference.Reference
import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Reference]
)

object Editor extends Element[Editor](
  elementName = "editor",
  parser = for {
    role <- Attribute("role").optional
    persName <- Reference.personParsable.optional
  } yield new Editor(
    role,
    persName
  )
) with ToXml[Editor] {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>
      {value.persName.map(reference => Reference.toXml(reference)).orNull}
    </editor>
}
