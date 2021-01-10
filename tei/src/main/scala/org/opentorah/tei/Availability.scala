package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, Element, Parsable, Parser, Xml}

final case class Availability(
  status: Option[String],
  xml: Seq[Xml.Node]
)

object Availability extends Element[Availability]("availability") {

  private val statusAttribute: Attribute.Optional[String] = Attribute("status").optional

  override def contentParsable: Parsable[Availability] = new Parsable[Availability] {
    override def parser: Parser[Availability] = for {
      status <- statusAttribute()
      xml <- Element.nodes()
    } yield new Availability(
      status,
      xml
    )

    override val antiparser: Antiparser[Availability] = Tei.concat(
      statusAttribute(_.status),
      Element.nodes(_.xml)
    )
  }
}
