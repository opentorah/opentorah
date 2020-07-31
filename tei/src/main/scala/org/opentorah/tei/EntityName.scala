package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}
import scala.xml.Node

final class EntityName private(
  val entityType: EntityType,
  val id: Option[String],
  val name: String
)

object EntityName extends ToXml[EntityName] {

  def parsable(entityType: EntityType): Parsable[EntityName] = new Element[EntityName](entityType.nameElement) {
    override protected def contentType: ContentType = ContentType.Characters

    override protected def parser: Parser[EntityName] = for {
      id <- Attribute.id.optional
      name <- org.opentorah.xml.Text().required
    } yield new EntityName(entityType, id, name)
  }


  override protected def elementName(value: EntityName): String = value.entityType.nameElement

  override protected def attributes(value: EntityName): Seq[Attribute.Value[_]] = Seq(
    Attribute.id.withValue(value.id)
  )

  override protected def content(value: EntityName): Seq[Node] = Seq(Xml.mkText(value.name))
}
