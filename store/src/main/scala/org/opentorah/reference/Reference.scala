package org.opentorah.reference

import org.opentorah.xml.{Attribute, ContentType, Element, Parser}
import scala.xml.{Elem, Node}

final case class Reference(
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
)

object Reference {

  final class ReferenceParsable(entity: Entity) extends Element[Reference](
    elementName = entity.nameElement,
    contentType = ContentType.Mixed,
    parser = parser(entity)
  ) {
    override def toXml(value: Reference): Elem =
      <name ref={value.ref.orNull} xml:id={value.id.orNull} role={value.role.orNull}>{value.name}</name>
        .copy(label = value.entity.nameElement)
  }

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

  final val personParsable = new ReferenceParsable(Entity.Person)
  final val organizationParsable = new ReferenceParsable(Entity.Organization)
  final val placeParsable = new ReferenceParsable(Entity.Place)

  def all(xml: Node): Seq[Reference] =
    Seq(personParsable, organizationParsable, placeParsable).flatMap(_.descendants(xml))
}
