package org.opentorah.tei

import org.opentorah.entity.EntityReference
import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class Editor(
  role: Option[String],
  persName: Option[EntityReference]
)

object Editor extends Element.WithToXml[Editor]("editor") {

  private val roleAttribute: Attribute[String] = Attribute("role")

  override protected def parser: Parser[Editor] = for {
    role <- roleAttribute.optional
    persName <- EntityReference.personParsable.optional
  } yield new Editor(
    role,
    persName
  )

  override protected def attributes(value: Editor): Seq[Attribute.Value[_]] = Seq(
    roleAttribute.withValue(value.role)
  )

  override protected def content(value: Editor): Seq[Elem] =
    value.persName.toSeq.map(EntityReference.toXml)
}
