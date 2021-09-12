package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser, ScalaXml}

final class Creation(
  val date: Date,
  val xml: ScalaXml.Nodes
)

object Creation extends Element[Creation]("creation"):

  override def contentParsable: Parsable[Creation] = new Parsable[Creation]:
    override def parser: Parser[Creation] = for
      date: Date <- Date.required()
      xml: ScalaXml.Nodes <- Element.nodes()
    yield Creation(
      date,
      xml
    )

    override val unparser: Unparser[Creation] = Tei.concat(
      Date.required(_.date),
      Element.nodes(_.xml)
    )
