package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
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

  override protected def attributes(value: Creation): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: Creation): Seq[Node] =
    Seq(Date.toXml(value.date)) ++ value.xml
}
