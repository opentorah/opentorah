package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, Element, Parsable, Parser}

final case class Editor(
  role: Option[String],
  persName: Option[EntityReference]
)

object Editor extends Element[Editor]("editor") {

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override def contentParsable: Parsable[Editor] = new Parsable[Editor] {
    override def parser: Parser[Editor] = for {
      role <- roleAttribute()
      persName <- EntityReference.Person.optional()
    } yield new Editor(
      role,
      persName
    )

    override val unparser: Unparser[Editor] = Tei.concat(
      roleAttribute(_.role),
      EntityReference.Person.optional(_.persName)
    )
  }
}
