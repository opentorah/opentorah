package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, Parsable, Parser, ScalaXml, Xml}

final class EntityReference(
  val entityType: EntityType,
  val name: ScalaXml.Nodes,
  val id: Option[String],
  val role: Option[String],
  val ref: Option[String]
)

object EntityReference extends EntityRelated[EntityReference](
  elementName = _.nameElement,
  entityType = _.entityType
):
  override protected def contentType: ContentType = ContentType.Mixed

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val refAttribute: Attribute.Optional[String] = Attribute("ref").optional
  private val typeAttribute: Attribute.Optional[String] = Attribute("type").optional

  override protected def parsable(entityType: EntityType): Parsable[EntityReference] = new Parsable[EntityReference]:
    override protected def parser: Parser[EntityReference] = for
      id: Option[String] <- idAttribute()
      role: Option[String] <- roleAttribute()
      ref: Option[String] <- refAttribute()
      _ <- typeAttribute() // We don't do anything with the type yet...
      name: ScalaXml.Nodes <- Element.nodes()
    yield EntityReference(
      entityType,
      name,
      id,
      role,
      ref
    )

    override def unparser: Unparser[EntityReference] = Tei.concat(
      refAttribute(_.ref),
      idAttribute(_.id),
      roleAttribute(_.role),
      Element.nodes(_.name)
    )
