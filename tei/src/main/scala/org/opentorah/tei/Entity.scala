package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}

final case class Entity private(
  id: Option[String],
  entityType: EntityType,
  role: Option[String],
  names: Seq[EntityName],
  content: Seq[Xml.Node]
) {
  def name: String = names.head.name

  def entityName: EntityName = names.head.copy(ref = Some(id.get))
}

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
) {
  override def toString: String = "Entity"

  override protected def contentType: ContentType = ContentType.Elements

  private val roleAttribute: Attribute[String] = Attribute("role")

  override protected def parser(entityType: EntityType): Parser[Entity] = for {
    id <- Xml.idAttribute.optional
    role <- roleAttribute.optional
    names <- EntityName.forEntityType(entityType).all
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Element.allNodes
  } yield new Entity(
    id,
    entityType,
    role,
    names,
    content,
  )

  override protected def antiparser(entityType: EntityType): Antiparser[Entity] = Tei.concat(
    Xml.idAttribute.toXmlOption.compose(_.id),
    roleAttribute.toXmlOption.compose(_.role),
    EntityName.forEntityType(entityType).toXmlSeq.compose(_.names),
    Antiparser.xml.compose(_.content)
  )
}
