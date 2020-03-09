package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, From, Parser, XmlUtil}
import scala.xml.{Elem, Node}

final case class Reference(
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object Reference {

  // TODO ZIOize
  def apply(
    entity: Entity,
    xml: Elem
  ): Reference = Parser.parseDo(From.xml(xml).parse(ContentType.Mixed, parser(entity)))

  private def parser(
    entity: Entity,
  ): Parser[Reference] = for {
    id <- Attribute.id.optional
    role <- Attribute("role").optional
    ref <- Attribute("ref").optional
    _ <- Attribute("type").optional // TODO we don't do anything with the type yet
    name <- Parser.allNodes
  } yield new Reference(
    entity,
    name,
    id,
    role,
    ref
  )

  def all(xml: Node): Seq[Reference] = for {
    entity <- Entity.values
    elem <- XmlUtil.descendants(xml, entity.nameElement)
  } yield Reference(entity, elem)
}
