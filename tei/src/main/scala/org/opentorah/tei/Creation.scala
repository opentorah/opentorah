package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser, Xml}

final case class Creation(
  date: Date,
  xml: Seq[Xml.Node]
)

object Creation extends Element[Creation]("creation") {

  override def parser: Parser[Creation] = for {
    date <- Date.required
    xml <- Element.allNodes
  } yield new Creation(
    date,
    xml
  )

  override val antiparser: Antiparser[Creation] = Tei.concat(
    Date.toXml(_.date),
    Antiparser.xml(_.xml)
  )
}
