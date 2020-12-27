package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, Element, Parser}

final case class Editor(
  role: Option[String],
  persName: Option[EntityReference]
)

object Editor extends Element[Editor]("editor") {

  private val roleAttribute: Attribute[String] = Attribute("role")

  override def parser: Parser[Editor] = for {
    role <- roleAttribute.optional
    persName <- EntityReference.Person.optional
  } yield new Editor(
    role,
    persName
  )

  override val antiparser: Antiparser[Editor] = Tei.concat(
    roleAttribute.toXmlOption(_.role),
    EntityReference.Person.toXmlOption(_.persName)
  )
}
