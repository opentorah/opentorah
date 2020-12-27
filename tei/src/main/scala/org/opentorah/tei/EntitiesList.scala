package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Parser, Xml}

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

  private val roleAttribute: Attribute[String] = Attribute("role")

  override protected def parser(entityType: EntityType): Parser[EntitiesList] = for {
    id <- Xml.idAttribute.required
    role <- roleAttribute.optional
    head <- org.opentorah.xml.Text("head").required
  } yield EntitiesList(
    entityType,
    id,
    role,
    head,
    Seq.empty
  )

  override protected def antiparser(entityType: EntityType): Antiparser[EntitiesList] = Antiparser.concat(
    Xml.idAttribute.toXml(_.id),
    roleAttribute.toXmlOption(_.role),
    Antiparser.xml(value => Seq(<head>{value.head}</head>))
  )
}
