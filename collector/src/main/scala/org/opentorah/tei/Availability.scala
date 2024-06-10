package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser, Xml}

final class Availability(
  val status: Option[String],
  val xml: Xml.Nodes
)

object Availability extends Element[Availability]("availability"):

  private val statusAttribute: Attribute.Optional[String] = Attribute("status").optional

  override def contentParsable: Parsable[Availability] = new Parsable[Availability]:
    override def parser: Parser[Availability] = for
      status: Option[String] <- statusAttribute()
      xml: Xml.Nodes <- Xml.nodes()
    yield Availability(
      status,
      xml
    )

    override val unparser: Unparser[Availability] = Tei.concat(
      statusAttribute(_.status),
      Xml.nodes(_.xml)
    )
