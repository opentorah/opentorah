package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}

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

object EntitiesList {
  private val roleAttribute: Attribute[String] = Attribute("role")

  override def toString: String = "EntitiesList"

  final val parsable: Parsable[EntitiesList] with ToXml[EntitiesList] = Parsable.union[EntitiesList](
    _.entityType.listElement,
    EntityType.values.map(mkParsable)
  )

  private def mkParsable(entityType: EntityType): Element.WithToXml[EntitiesList] =
    new Element.WithToXml[EntitiesList](entityType.listElement) {
      override def contentType: ContentType = ContentType.Elements

      override def parser: Parser[EntitiesList] = for {
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

      override protected val antiparser: Antiparser[EntitiesList] = Antiparser.concat(
        Xml.idAttribute.toXml.compose(_.id),
        roleAttribute.toXmlOption.compose(_.role),
        Antiparser.xml.compose(value => Seq(<head>{value.head}</head>))
      )
    }
}
