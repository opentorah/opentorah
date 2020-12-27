package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, Element, Parser, Xml}

final case class Availability(
  status: Option[String],
  xml: Seq[Xml.Node]
)

object Availability extends Element[Availability]("availability") {

  private val statusAttribute: Attribute[String] = Attribute("status")

  override def parser: Parser[Availability] = for {
    status <- statusAttribute.optional
    xml <- Element.allNodes
  } yield new Availability(
    status,
    xml
  )

  override val antiparser: Antiparser[Availability] = Tei.concat(
    statusAttribute.toXmlOption(_.status),
    Antiparser.xml(_.xml)
  )
}
