package org.opentorah.tei

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser, Xml}

final class Entity(
  val id: Option[String],
  val entityType: EntityType,
  val role: Option[String],
  val names: Seq[EntityName],
  val content: Element.Nodes // TODO rename body?
):
  def name: String = names.head.name

  def entityName: EntityName = names.head.copy(ref = Some(id.get))

  def copy(
    id: Option[String] = id,
    entityType: EntityType = entityType,
    role: Option[String] = role,
    names: Seq[EntityName] = names,
    content: Element.Nodes = content
  ): Entity = Entity(
    id,
    entityType,
    role,
    names,
    content
  )

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
):
  override def toString: String = "Entity"

  override protected def contentType: Element.ContentType = Element.ContentType.Elements

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override protected def parsable(entityType: EntityType): Parsable[Entity] = new Parsable[Entity]:
    private val namesParsable: Parsable[Seq[EntityName]] = EntityName.forEntityType(entityType).seq

    override protected def parser: Parser[Entity] = for
      id: Option[String] <- idAttribute()
      role: Option[String] <- roleAttribute()
      names: Seq[EntityName] <- namesParsable()
      _ <- Effects.check(names.nonEmpty, s"No names in $id")
      content: Element.Nodes <- Element.nodes()
    yield Entity(
      id,
      entityType,
      role,
      names,
      content,
    )

    override def unparser: Unparser[Entity] = Tei.concat(
      idAttribute(_.id),
      roleAttribute(_.role),
      namesParsable(_.names),
      Element.nodes(_.content)
    )
