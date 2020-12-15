package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, From, Parser, Xml}

final case class EntityReference(
  entityType: EntityType,
  name: Seq[Xml.Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
) {
  def text: String = name.map(_.text.trim).mkString(" ")
}

object EntityReference extends EntityRelated[EntityReference](
  elementName = _.nameElement,
  entityType = _.entityType
) {
  override protected def contentType: ContentType = ContentType.Mixed

  private val roleAttribute: Attribute[String] = Attribute("role")
  private val refAttribute: Attribute[String] = Attribute("ref")
  private val typeAttribute: Attribute[String] = Attribute("type")

  override protected def parser(entityType: EntityType): Parser[EntityReference] = for {
    id <- Xml.idAttribute.optional
    role <- roleAttribute.optional
    ref <- refAttribute.optional
    _ <- typeAttribute.optional // We don't do anything with the type yet...
    name <- Element.allNodes
  } yield new EntityReference(
    entityType,
    name,
    id,
    role,
    ref
  )

  override protected def antiparser(entityType: EntityType): Antiparser[EntityReference] = Tei.concat(
    refAttribute.toXmlOption.compose(_.ref),
    Xml.idAttribute.toXmlOption.compose(_.id),
    roleAttribute.toXmlOption.compose(_.role),
    Antiparser.xml.compose(_.name)
  )

  final def from(xml: Seq[Xml.Node]): Seq[EntityReference] =
    EntityType.values.flatMap(entityType => xml.flatMap(node =>
      Xml.descendants(node, entityType.nameElement)
        .map(descendant => Parser.parseDo(parse(From.xml("descendants", descendant))))
    ))
}
