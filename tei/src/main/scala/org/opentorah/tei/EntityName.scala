package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}

final case class EntityName(
  entityType: EntityType,
  id: Option[String] = None,
  ref: Option[String] = None,
  name: String
)

object EntityName {

  val refAttribute: Attribute[String] = Attribute("ref")

  private def parsable(entityType: EntityType): Element.WithToXml[EntityName] = new Element.WithToXml[EntityName](entityType.nameElement) {
    override def contentType: ContentType = ContentType.Characters

    override def parser: Parser[EntityName] = for {
      id <- Xml.idAttribute.optional
      ref <- refAttribute.optional
      name <- org.opentorah.xml.Text().required
    } yield EntityName(entityType, id, ref, name)

    override protected def antiparser: Antiparser[EntityName] = Tei.concat(
      Xml.idAttribute.toXmlOption.compose[EntityName](_.id),
      refAttribute.toXmlOption.compose[EntityName](_.ref),
      Antiparser.xml.compose[EntityName](value => Seq(Xml.mkText(value.name)))
    )
  }

  private val personParsable: Element.WithToXml[EntityName] = parsable(EntityType.Person)
  private val organizationParsable: Element.WithToXml[EntityName] = parsable(EntityType.Organization)
  private val placeParsable: Element.WithToXml[EntityName] = parsable(EntityType.Place)

  final val parsable: Parsable[EntityName] with ToXml[EntityName] = Parsable.union[EntityName](
    _.entityType.nameElement,
    Seq(personParsable, organizationParsable, placeParsable)
  )

  def getParsable(entityType: EntityType): Element.WithToXml[EntityName] = entityType match {
    case EntityType.Person       => personParsable
    case EntityType.Organization => organizationParsable
    case EntityType.Place        => placeParsable
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
