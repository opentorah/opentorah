package org.digitaljudaica.tei

import org.digitaljudaica.xml.{Descriptor, Xml}
import scala.xml.{Elem, Node}

final case class Creation(
  date: Date,
  xml: Seq[Node]
)

object Creation extends Descriptor[Creation](
  elementName = "creation",
  contentParser = for {
    date <- Date.required
    xml <- Xml.allNodes
  } yield new Creation(
    date,
    xml
  )
) {
  override def toXml(value: Creation): Elem =
    <creation>{Date.toXml(value.date)}{value.xml}</creation>
}
