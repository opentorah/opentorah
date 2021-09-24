package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser, Xml}

final class EntityName(
  val entityType: EntityType,
  val id: Option[String] = None,
  val ref: Option[String] = None,
  val name: String
):
  def copy(
    entityType: EntityType = entityType,
    id: Option[String] = id,
    ref: Option[String] = ref,
    name: String = name
  ): EntityName = EntityName(
    entityType,
    id,
    ref,
    name
  )

object EntityName extends EntityRelated[EntityName](
  elementName = _.nameElement,
  entityType = _.entityType
):

  override protected def contentType: Element.ContentType = Element.ContentType.Characters

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  val refAttribute: Attribute.Optional[String] = Attribute("ref").optional
  private val textParsable: Parsable[String] = org.opentorah.xml.Text().required

  override protected def parsable(entityType: EntityType): Parsable[EntityName] = new Parsable[EntityName]:
    override protected def parser: Parser[EntityName] = for
      id: Option[String] <- idAttribute()
      ref: Option[String] <- refAttribute()
      name: String <- textParsable()
    yield EntityName(
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
