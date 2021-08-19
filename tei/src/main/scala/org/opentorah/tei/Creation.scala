package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser, ScalaXml}

final case class Creation(
  date: Date,
  xml: ScalaXml.Nodes
)

object Creation extends Element[Creation]("creation") {

  override def contentParsable: Parsable[Creation] = new Parsable[Creation] {
    override def parser: Parser[Creation] = for {
      date <- Date.required()
      xml <- Element.nodes()
    } yield new Creation(
      date,
      xml
    )

    override val unparser: Unparser[Creation] = Tei.concat(
      Date.required(_.date),
      Element.nodes(_.xml)
    )
  }
}
