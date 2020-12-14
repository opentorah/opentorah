package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, From, Parsable, Parser, ToXml, Xml}

final case class EntityReference(
  entityType: EntityType,
  name: Seq[Xml.Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
) {
  def text: String = name.map(_.text.trim).mkString(" ")
}

object EntityReference {

  private val roleAttribute: Attribute[String] = Attribute("role")
  private val refAttribute: Attribute[String] = Attribute("ref")
  private val typeAttribute: Attribute[String] = Attribute("type")

  val personParsable: Element.WithToXml[EntityReference] = mkParsable(EntityType.Person)

  final val parsable: Parsable[EntityReference] with ToXml[EntityReference] = Parsable.union[EntityReference](
    _.entityType.nameElement,
    Seq(personParsable, mkParsable(EntityType.Organization), mkParsable(EntityType.Place))
  )

  private def mkParsable(entityType: EntityType): Element.WithToXml[EntityReference] =
    new Element.WithToXml[EntityReference](entityType.nameElement) {
      override def contentType: ContentType = ContentType.Mixed

      override def parser: Parser[EntityReference] = for {
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

      override protected def antiparser: Antiparser[EntityReference] = Tei.concat(
        refAttribute.toXmlOption.compose(_.ref),
        Xml.idAttribute.toXmlOption.compose(_.id),
        roleAttribute.toXmlOption.compose(_.role),
        Antiparser.xml.compose(_.name)
      )
    }

  final def from(xml: Seq[Xml.Node]): Seq[EntityReference] = EntityType.values.flatMap(entityType => xml.flatMap(node =>
    Xml.descendants(node, entityType.nameElement)
      .map(descendant => Parser.parseDo(parsable.parse(From.xml("descendants", descendant))))
  ))
}
