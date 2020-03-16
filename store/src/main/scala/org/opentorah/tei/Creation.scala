package org.opentorah.tei

import org.opentorah.xml.{Element, Parser, ToXml}
import scala.xml.{Elem, Node}

final case class Creation(
  date: Date,
  xml: Seq[Node]
)

object Creation extends Element[Creation](
  elementName = "creation",
  parser = for {
    date <- Date.required
    xml <- Parser.allNodes
  } yield new Creation(
    date,
    xml
  )
) with ToXml[Creation] {
  override def toXml(value: Creation): Elem =
    <creation>{Date.toXml(value.date)}{value.xml}</creation>
}
