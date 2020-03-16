package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, Parsable, Parser, Text, ToXml}
import scala.xml.Elem

final case class NamesList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String
) {
  def includes(named: Named): Boolean = (named.entity == entity) && (named.role == role)
}

object NamesList extends Parsable[NamesList] with ToXml[NamesList] {

  override def contentType: ContentType = ContentType.Elements

  override def name2parser(elementName: String): Option[Parser[NamesList]] =
    Entity.forList(elementName).map(contentParser)

  private def contentParser(entity: Entity): Parser[NamesList] = for {
    id <- Attribute.id.required
    role <- Attribute("role").optional
    head <- Text("head").required
  } yield NamesList(
    entity,
    id,
    role,
    head
  )

  override def toXml(value: NamesList): Elem =
    <elem id={value.id} role={value.role.orNull}>
      <head>{value.head}</head>
    </elem>
    .copy(label = value.entity.listElement)
}
