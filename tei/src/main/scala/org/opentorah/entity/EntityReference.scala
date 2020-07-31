package org.opentorah.entity

import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Parser, ToXml, UnionParsable}
import scala.xml.Node

final case class EntityReference(
  entityType: EntityType,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object EntityReference extends ToXml[EntityReference] {

  private val roleAttribute: Attribute[String] = Attribute("role")
  private val refAttribute: Attribute[String] = Attribute("ref")
  private val typeAttribute: Attribute[String] = Attribute("type")

  private def parsable(entityType: EntityType): Parsable[EntityReference] = new Element[EntityReference](entityType.nameElement) {
    override protected def contentType: ContentType = ContentType.Mixed

    override protected def parser: Parser[EntityReference] = for {
      id <- Attribute.id.optional
      role <- roleAttribute.optional
      ref <- refAttribute.optional
      _ <- typeAttribute.optional // We don't do anything with the type yet...
      name <- Element.allNodes
    } yield new EntityReference(
      entityType,
      name,
      id,
      role,
      ref
    )
  }

  final val personParsable: Parsable[EntityReference] = parsable(EntityType.Person)
  final val organizationParsable: Parsable[EntityReference] = parsable(EntityType.Organization)
  final val placeParsable: Parsable[EntityReference] = parsable(EntityType.Place)

  final val parsable: UnionParsable[EntityReference] = new UnionParsable[EntityReference](Seq(
    personParsable, organizationParsable, placeParsable
  ))

  final def from(xml: Seq[Node]): Seq[EntityReference] =
    xml.flatMap(parsable.descendants)

  override protected def elementName(value: EntityReference): String = value.entityType.nameElement

  override protected def attributes(value: EntityReference): Seq[Attribute.Value[_]] = Seq(
    refAttribute.withValue(value.ref),
    Attribute.id.withValue(value.id),
    roleAttribute.withValue(value.role)
  )

  override protected def content(value: EntityReference): Seq[Node] = value.name
}