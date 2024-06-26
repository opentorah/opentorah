package org.opentorah.tei

import org.opentorah.xml.{Attribute, ElementTo, Parsable, Parser, Unparser}

final class Editor(
  val role: Option[String],
  val persName: Option[EntityReference]
)

object Editor extends ElementTo[Editor]("editor"):

  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override def contentParsable: Parsable[Editor] = new Parsable[Editor]:
    override def parser: Parser[Editor] = for
      role: Option[String] <- roleAttribute()
      persName: Option[EntityReference] <- EntityReference.Person.optional()
    yield Editor(
      role,
      persName
    )

    override val unparser: Unparser[Editor] = Tei.concat(
      roleAttribute(_.role),
      EntityReference.Person.optional(_.persName)
    )
