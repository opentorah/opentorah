package org.opentorah.tei

import org.opentorah.reference.Reference
import org.opentorah.xml.{Attribute, Element}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[Reference]
)

object Editor extends Element[Editor](
  elementName = "editor",
  parser = for {
    role <- Attribute("role").optional
    persName <- Reference.persReference.optional
  } yield new Editor(
    role,
    persName
  )
) {
  override def toXml(value: Editor): Elem =
    <editor role={value.role.orNull}>{value.persName.map(reference => Reference.parsable.toXml(reference)).orNull}</editor>
}
