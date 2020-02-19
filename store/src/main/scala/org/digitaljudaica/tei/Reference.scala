package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.{From, Ops, Parser, Xml}
import scala.xml.{Elem, Node}

final case class Reference(
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
) {

  def toXml: Elem =
    <name ref={ref.orNull} xml:id={id.orNull} role={role.orNull}>{name}</name>
      .copy(label = entity.nameElement)
}

object Reference {
  def apply(
    entity: Entity,
    xml: Elem
  ): Reference = From.xml(xml).mixed.parseDo(parser(entity))

  private def parser(
    entity: Entity,
  ): Parser[Reference] = for {
    id <- Xml.attribute.optional.id
    role <- Xml.attribute.optional("role")
    ref <- Xml.attribute.optional("ref")
    _ <- Xml.attribute.optional("type") // TODO we don't do anything with the type yet
    name <- Xml.allNodes
  } yield new Reference(
    entity,
    name,
    id,
    role,
    ref
  )

  def all(xml: Elem): Seq[Reference] = for {
    entity <- Entity.values
    elem <- Ops.descendants(xml, entity.nameElement)
  } yield Reference(entity, elem)
}
