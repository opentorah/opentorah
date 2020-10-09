package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, UnionParsable, Xml}
import scala.xml.Node

final case class EntityReference(
  entityType: EntityType,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object EntityReference extends ToXml[EntityReference] {

  private val roleAttribute: Attribute[String] = Attribute("role")
  private val refAttribute: Attribute[String] = Attribute("ref")
  private val typeAttribute: Attribute[String] = Attribute("type")

  private def parsable(entityType: EntityType): Parsable[EntityReference] = new Element[EntityReference](entityType.nameElement) {
    override protected def contentType: ContentType = ContentType.Mixed

    override protected def parser: Parser[EntityReference] = for {
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
  }

  final val personParsable: Parsable[EntityReference] = parsable(EntityType.Person)
  final val organizationParsable: Parsable[EntityReference] = parsable(EntityType.Organization)
  final val placeParsable: Parsable[EntityReference] = parsable(EntityType.Place)

  final val parsable: UnionParsable[EntityReference] = new UnionParsable[EntityReference](Seq(
    personParsable, organizationParsable, placeParsable
  ))

  final def from(xml: Seq[Node]): Seq[EntityReference] =
    xml.flatMap(parsable.descendants)

  override protected def elementName(value: EntityReference): String = value.entityType.nameElement

  override protected val antiparser: Antiparser[EntityReference] = Antiparser.concatWithNamespace(Tei.namespace,
    refAttribute.toXmlOption.compose(_.ref),
    Xml.idAttribute.toXmlOption.compose(_.id),
    roleAttribute.toXmlOption.compose(_.role),
    Antiparser.xml.compose(_.name)
  )
}
