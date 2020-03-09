package org.opentorah.reference

import org.opentorah.xml.{Attribute, Element, Parser, Repeatable, Text}
import zio.ZIO

final case class NamesList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String
) {
  def includes(named: Named): Boolean = (named.entity == entity) && (named.role == role)
}

object NamesList extends Repeatable[NamesList] {

  // TODO generalize name recognizer; use it in both optional() and required() - to get better error messages!
  def optional: Parser[Option[NamesList]] = for {
    name <- Element.nextName
    result <- if (name.isEmpty) ZIO.none else {
      val entityO = Entity.forList(name.get)
      if (entityO.isEmpty) ZIO.none else for {
        result <- Element(name.get, contentParser(entityO.get)).required
      } yield Some(result)
    }
  } yield result

  private def contentParser(entity: Entity): Parser[NamesList] = for {
    id <- Attribute.id.required
    role <- Attribute("role").optional
    head <- Text("head").required
  } yield NamesList(
    entity,
    id,
    role,
    head
  )
}
