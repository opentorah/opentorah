package org.opentorah.reference

import org.opentorah.xml.{Attribute, Element, Parser, Text, Xml}

// TODO extend Parsable
final case class NamesList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String
) {
  def includes(named: Named): Boolean = (named.entity == entity) && (named.role == role)
}

object NamesList {

  val parser: Parser[NamesList] = for {
    name <- Xml.name
    entity: Entity = Entity.forList(name).get
    id <- Attribute.id.required
    role <- Attribute("role").optional
    head <- Text("head").required
  } yield NamesList(
    entity,
    id,
    role,
    head
  )

  val all: Parser[Seq[NamesList]] = Element(parser).all
}
