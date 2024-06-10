package org.opentorah.tei

import org.opentorah.xml.{Element, Parsable, Parser, Unparser, Xml}

final class Creation(
  val date: Date,
  val xml: Xml.Nodes
)

object Creation extends Element[Creation]("creation"):

  override def contentParsable: Parsable[Creation] = new Parsable[Creation]:
    override def parser: Parser[Creation] = for
      date: Date <- Date.required()
      xml: Xml.Nodes <- Xml.nodes()
    yield Creation(
      date,
      xml
    )

    override val unparser: Unparser[Creation] = Tei.concat(
      Date.required(_.date),
      Xml.nodes(_.xml)
    )
