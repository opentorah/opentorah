package org.digitaljudaica.reference

import org.digitaljudaica.xml.{Element, Parser, Xml}

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
    id <- Xml.attribute.required.id
    role <- Xml.attribute.optional("role")
    head <- Xml.text.required("head")
  } yield NamesList(
    entity,
    id,
    role,
    head
  )

  val all: Parser[Seq[NamesList]] = Element(parser).all
}
