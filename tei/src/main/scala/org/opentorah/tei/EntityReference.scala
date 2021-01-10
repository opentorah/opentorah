package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, From, Parsable, Parser, Xml}

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

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional
  private val refAttribute: Attribute.Optional[String] = Attribute("ref").optional
  private val typeAttribute: Attribute.Optional[String] = Attribute("type").optional

  override protected def parsable(entityType: EntityType): Parsable[EntityReference] = new Parsable[EntityReference] {
    override protected def parser: Parser[EntityReference] = for {
      id <- idAttribute()
      role <- roleAttribute()
      ref <- refAttribute()
      _ <- typeAttribute() // We don't do anything with the type yet...
      name <- Element.nodes()
    } yield new EntityReference(
      entityType,
      name,
      id,
      role,
      ref
    )

    override def antiparser: Antiparser[EntityReference] = Tei.concat(
      refAttribute(_.ref),
      idAttribute(_.id),
      roleAttribute(_.role),
      Element.nodes(_.name)
    )
  }

  final def from(xml: Seq[Xml.Node]): Seq[EntityReference] =
    EntityType.values.flatMap(entityType => xml.flatMap(node =>
      Xml.descendants(node, entityType.nameElement)
        .map(descendant => Parser.parseDo(parse(From.xml("descendants", descendant))))
    ))
}
