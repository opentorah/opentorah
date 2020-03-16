package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, ToXml}
import scala.xml.{Elem, Node}

final case class Availability(
  status: Option[String],
  xml: Seq[Node]
)

object Availability extends Element[Availability](
  elementName = "availability",
  parser = for {
    status <- Attribute("status").optional
    xml <- Element.allNodes
  } yield new Availability(
    status,
    xml
  )
) with ToXml[Availability] {
  def apply(status: String, xml: Seq[Node]): Availability = new Availability(
    status = Some(status),
    xml
  )

  override def toXml(value: Availability): Elem =
    <availability status={value.status.orNull}>{value.xml}</availability>
}
