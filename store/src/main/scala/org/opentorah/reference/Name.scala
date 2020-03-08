package org.opentorah.reference

import org.opentorah.xml.{Attribute, Parser, Text, Xml}
import scala.xml.Elem

final class Name private(
  entity: Entity,
  val id: Option[String],
  val name: String
) {
  def toXml: Elem = {
    <name xml:id={id.orNull}>{name}</name>
      .copy(label = entity.nameElement)
  }
}

object Name {
  def parser(entity: Entity): Parser[Name] = for {
    _ <- Xml.checkName(entity.nameElement)
    id <- Attribute.id.optional
    name <- Text().required
  } yield new Name(entity, id, name)
}
