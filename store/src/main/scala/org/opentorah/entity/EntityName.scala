package org.opentorah.entity

import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Text, ToXml}
import scala.xml.Elem

final class EntityName private(
  val entityType: EntityType,
  val id: Option[String],
  val name: String
)

object EntityName extends ToXml[EntityName] {

  def parsable(entityType: EntityType): Parsable[EntityName] = new Element[EntityName](
    elementName = entityType.nameElement,
    contentType = ContentType.Characters,
    parser = for {
      id <- Attribute.id.optional
      name <- Text().required
    } yield new EntityName(entityType, id, name)
  )

  override def toXml(value: EntityName): Elem = {
    <name xml:id={value.id.orNull}>{value.name}</name>
      .copy(label = value.entityType.nameElement)
  }
}
