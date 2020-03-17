package org.opentorah.reference

import org.opentorah.store.Path
import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, ToXml, UnionParsable}
import scala.xml.{Elem, Node}

final case class Reference(
  source: Path,
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
) {
  def at(path: Path): Reference = copy(source = path ++ this.source)
}

object Reference extends ToXml[Reference] {

  private def parsable(entity: Entity): Parsable[Reference] = new Element[Reference](
    elementName = entity.nameElement,
    contentType = ContentType.Mixed,
    parser = for {
      id <- Attribute.id.optional
      role <- Attribute("role").optional
      ref <- Attribute("ref").optional
      _ <- Attribute("type").optional // TODO we don't do anything with the type yet
      name <- Element.allNodes
    } yield new Reference(
      Path.empty,
      entity,
      name,
      id,
      role,
      ref
    )
  )

  final val personParsable: Parsable[Reference] = parsable(Entity.Person)
  final val organizationParsable: Parsable[Reference] = parsable(Entity.Organization)
  final val placeParsable: Parsable[Reference] = parsable(Entity.Place)

  final val parsable = new UnionParsable[Reference](Seq(
    personParsable, organizationParsable, placeParsable
  ))

  override def toXml(value: Reference): Elem =
    <name ref={value.ref.orNull} xml:id={value.id.orNull} role={value.role.orNull}>{value.name}</name>
      .copy(label = value.entity.nameElement)
}
