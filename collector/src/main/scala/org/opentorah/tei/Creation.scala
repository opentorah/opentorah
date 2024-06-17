package org.opentorah.tei

import org.opentorah.xml.{ElementTo, Nodes, Parsable, Parser, Unparser}

final class Creation(
  val date: Date,
  val xml: Nodes
)

object Creation extends ElementTo[Creation]("creation"):

  override def contentParsable: Parsable[Creation] = new Parsable[Creation]:
    override def parser: Parser[Creation] = for
      date: Date <- Date.required()
      xml: Nodes <- Nodes.all()
    yield Creation(
      date,
      xml
    )

    override val unparser: Unparser[Creation] = Tei.concat(
      Date.required(_.date),
      Nodes.all(_.xml)
    )
