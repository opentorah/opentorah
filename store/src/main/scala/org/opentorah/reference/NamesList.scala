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

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[NamesList]] =
    (for (entity <- Entity.values)
      yield entity.listElement -> new Parsable.ContentTypeAndParser[NamesList](ContentType.Elements, contentParser(entity))).toMap

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
