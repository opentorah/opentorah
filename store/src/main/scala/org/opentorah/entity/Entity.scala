package org.opentorah.entity

import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}
import scala.xml.{Elem, Node}

final case class Entity private(
  id: Option[String],
  entityType: EntityType,
  role: Option[String],
  names: Seq[EntityName],
  content: Seq[Node]
) {

  def name: String = names.head.name

  def references: Seq[EntityReference] = EntityReference.from(content)
}

object Entity extends Parsable[Entity] with ToXml[Entity] {

  override def toString: String = "Entity"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[Entity]] = EntityType.values.map { entity =>
    entity.element -> new Parsable.ContentTypeAndParser[Entity](ContentType.Elements, parser(entity))
  }.toMap

  private def parser(entityType: EntityType): Parser[Entity] = for {
    id <- Attribute("id").optional
    role <- Attribute("role").optional
    names <- EntityName.parsable(entityType).all
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Element.allNodes
  } yield new Entity(
    id,
    entityType,
    role,
    names,
    content = content.map(Xml.removeNamespace),
  )

  override def toXml(value: Entity): Elem = {
    <elem xml:id={value.id.orNull} role={value.role.orNull}>
      {value.names.map(EntityName.toXml)}
      {value.content}
    </elem>
      .copy(label = value.entityType.element)
  }
}
