package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}
import scala.xml.Node

final case class Creation(
  date: Date,
  xml: Seq[Node]
)

object Creation extends Element.WithToXml[Creation]("creation") {

  override protected def parser: Parser[Creation] = for {
    date <- Date.required
    xml <- Element.allNodes
  } yield new Creation(
    date,
    xml
  )

  override protected val antiparser: Antiparser[Creation] = Antiparser(
    Date.elementAntiparser.compose[Creation](_.date),
    Antiparser.xml.compose[Creation](_.xml)
  )
}
