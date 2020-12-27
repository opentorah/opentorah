package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Parser, Xml}

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

  val refAttribute: Attribute[String] = Attribute("ref")

  override protected def parser(entityType: EntityType): Parser[EntityName] = for {
    id <- Xml.idAttribute.optional
    ref <- refAttribute.optional
    name <- org.opentorah.xml.Text().required
  } yield EntityName(
    entityType,
    id,
    ref,
    name
  )

  override protected def antiparser(entityType: EntityType): Antiparser[EntityName] = Tei.concat(
    Xml.idAttribute.toXmlOption(_.id),
    refAttribute.toXmlOption(_.ref),
    Antiparser.xml(value => Seq(Xml.mkText(value.name)))
  )

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
