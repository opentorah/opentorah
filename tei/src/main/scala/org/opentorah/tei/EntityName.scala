package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}

final class EntityName(
  val entityType: EntityType,
  val id: Option[String] = None,
  val ref: Option[String] = None,
  val name: String
)

object EntityName extends ToXml[EntityName] {

  val refAttribute: Attribute[String] = Attribute("ref")

  def parsable(entityType: EntityType): Parsable[EntityName] = new Element[EntityName](entityType.nameElement) {
    override protected def contentType: ContentType = ContentType.Characters

    override protected def parser: Parser[EntityName] = for {
      id <- Xml.idAttribute.optional
      ref <- refAttribute.optional
      name <- org.opentorah.xml.Text().required
    } yield new EntityName(entityType, id, ref, name)
  }

  override protected def elementName(value: EntityName): String = value.entityType.nameElement

  override protected val antiparser: Antiparser[EntityName] = Antiparser.concatWithNamespace(Tei.namespace,
    Xml.idAttribute.toXmlOption.compose[EntityName](_.id),
    refAttribute.toXmlOption.compose[EntityName](_.ref),
    Antiparser.xml.compose[EntityName](value => Seq(Xml.mkText(value.name)))
  )
}
