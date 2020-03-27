package org.opentorah.entity

import org.opentorah.store.Path
import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, ToXml, UnionParsable}
import scala.xml.{Elem, Node}

final case class EntityReference(
  entityType: EntityType,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object EntityReference extends ToXml[EntityReference] {

  private def parsable(entityType: EntityType): Parsable[EntityReference] = new Element[EntityReference](
    elementName = entityType.nameElement,
    contentType = ContentType.Mixed,
    parser = for {
      id <- Attribute.id.optional
      role <- Attribute("role").optional
      ref <- Attribute("ref").optional
      _ <- Attribute("type").optional // TODO we don't do anything with the type yet
      name <- Element.allNodes
    } yield new EntityReference(
      entityType,
      name,
      id,
      role,
      ref
    )
  )

  final val personParsable: Parsable[EntityReference] = parsable(EntityType.Person)
  final val organizationParsable: Parsable[EntityReference] = parsable(EntityType.Organization)
  final val placeParsable: Parsable[EntityReference] = parsable(EntityType.Place)

  final val parsable = new UnionParsable[EntityReference](Seq(
    personParsable, organizationParsable, placeParsable
  ))

  override def toXml(value: EntityReference): Elem =
    <name ref={value.ref.orNull} xml:id={value.id.orNull} role={value.role.orNull}>{value.name}</name>
      .copy(label = value.entityType.nameElement)
}
