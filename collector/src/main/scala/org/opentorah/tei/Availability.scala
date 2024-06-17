package org.opentorah.tei

import org.opentorah.xml.{Attribute, ElementTo, Nodes, Parsable, Parser, Unparser}

final class Availability(
  val status: Option[String],
  val xml: Nodes
)

object Availability extends ElementTo[Availability]("availability"):

  private val statusAttribute: Attribute.Optional[String] = Attribute("status").optional

  override def contentParsable: Parsable[Availability] = new Parsable[Availability]:
    override def parser: Parser[Availability] = for
      status: Option[String] <- statusAttribute()
      xml: Nodes <- Nodes.all()
    yield Availability(
      status,
      xml
    )

    override val unparser: Unparser[Availability] = Tei.concat(
      statusAttribute(_.status),
      Nodes.all(_.xml)
    )
