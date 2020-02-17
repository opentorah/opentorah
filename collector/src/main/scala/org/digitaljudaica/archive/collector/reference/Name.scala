package org.digitaljudaica.archive.collector.reference

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}

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
    id <- Xml.attribute.optional.id
    name <- Xml.characters.required
  } yield new Name(entity, id, name)
}
