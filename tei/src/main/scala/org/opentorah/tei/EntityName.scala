package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, ContentType, Parsable, Parser, Xml}

final case class EntityName(
  entityType: EntityType,
  id: Option[String] = None,
  ref: Option[String] = None,
  name: String
)

object EntityName extends EntityRelated[EntityName](
  elementName = _.nameElement,
  entityType = _.entityType
) {

  override protected def contentType: ContentType = ContentType.Characters

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  val refAttribute: Attribute.Optional[String] = Attribute("ref").optional
  private val textParsable: Parsable[String] = org.opentorah.xml.Text().required

  override protected def parsable(entityType: EntityType): Parsable[EntityName] = new Parsable[EntityName] {
    override protected def parser: Parser[EntityName] = for {
      id <- idAttribute()
      ref <- refAttribute()
      name <- textParsable()
    } yield EntityName(
      entityType,
      id,
      ref,
      name
    )

    override def unparser: Unparser[EntityName] = Tei.concat(
      idAttribute(_.id),
      refAttribute(_.ref),
      textParsable(_.name)
    )
  }

  // TODO just the entity.entityName, like in the NamesObject?
  def forEntity(entity: Entity): EntityName = EntityName(
    entityType = entity.entityType,
    ref = entity.id,
    name = entity.id.getOrElse("")
  )

  def forReference(entityReference: EntityReference): EntityName = EntityName(
    entityType = entityReference.entityType,
    name = entityReference.text
  )
}
