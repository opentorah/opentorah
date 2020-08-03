package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Parsable, Parser, ToXml}

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

object EntitiesList extends Parsable[EntitiesList] with ToXml[EntitiesList] {
  private val roleAttribute: Attribute[String] = Attribute("role")

  override def toString: String = "EntitiesList"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[EntitiesList]] = EntityType.values.map { entity =>
    entity.listElement -> new Parsable.ContentTypeAndParser[EntitiesList](ContentType.Elements, parser(entity))
  }.toMap

  private def parser(entityType: EntityType): Parser[EntitiesList] = for {
    id <- Attribute.id.required
    role <- roleAttribute.optional
    head <- org.opentorah.xml.Text("head").required
  } yield EntitiesList(
    entityType,
    id,
    role,
    head,
    Seq.empty
  )


  override protected def elementName(value: EntitiesList): String = value.entityType.listElement

  override protected val antiparser: Antiparser[EntitiesList] = Antiparser(
    Attribute.id.toAntiparser.premap(_.id),
    roleAttribute.toAntiparserOption.premap(_.role),
    Antiparser.xml.premap(value => Seq(<head>{value.head}</head>))
  )
}
