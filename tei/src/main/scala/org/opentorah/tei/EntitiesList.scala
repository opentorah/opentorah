package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, Xml}

// TODO remove
final case class EntitiesList(
  entityType: EntityType,
  id: String,
  role: Option[String],
  head: String,
  entities: Seq[Entity]
) {
  def take(entities: Seq[Entity]): EntitiesList = copy(entities = entities.filter(includes))

  def includes(entity: Entity): Boolean = (entity.entityType == entityType) && (entity.role == role)

  def isEmpty: Boolean = entities.isEmpty
}

object EntitiesList extends EntityRelated[EntitiesList](
  elementName = _.listElement,
  entityType = _.entityType
) {
  override def toString: String = "EntitiesList"

  override protected def contentType: ContentType = ContentType.Elements

  private val idAttribute: Attribute.Required[String] = Xml.idAttribute.required
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val headElement: Parsable[String] = org.opentorah.xml.Text("head").required

  override protected def parsable(entityType: EntityType): Parsable[EntitiesList] = new Parsable[EntitiesList] {
    override protected def parser: Parser[EntitiesList] = for {
      id <- idAttribute()
      role <- roleAttribute()
      head <- headElement()
    } yield EntitiesList(
      entityType,
      id,
      role,
      head,
      Seq.empty
    )

    override def antiparser: Antiparser[EntitiesList] = Antiparser.concat(
      idAttribute(_.id),
      roleAttribute(_.role),
      headElement(_.head)
    )
  }
}
