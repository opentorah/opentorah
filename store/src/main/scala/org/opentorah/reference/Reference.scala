package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, From, Parsable, Parser, XmlUtil}
import scala.xml.{Elem, Node}

final case class Reference(
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object Reference {

  sealed class ReferenceParsable extends Parsable[Reference] {
    override def contentType: ContentType = ContentType.Mixed

    override def name2parser(elementName: String): Option[Parser[Reference]] =
      Entity.forName(elementName).flatMap { entity =>
        if (isAcceptable(entity)) Some(parser(entity)) else None
      }

    protected def isAcceptable(entity: Entity): Boolean = true

    override def toXml(value: Reference): Elem =
      <name ref={value.ref.orNull} xml:id={value.id.orNull} role={value.role.orNull}>{value.name}</name>
        .copy(label = value.entity.nameElement)
  }

  object parsable extends ReferenceParsable

  private def parser(entity: Entity): Parser[Reference] = for {
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
  } yield Parser.parseDo(From.xml(elem).parse(parsable))

  object persReference extends ReferenceParsable {
    override protected def isAcceptable(entity: Entity): Boolean = entity == Entity.Person
  }
}
