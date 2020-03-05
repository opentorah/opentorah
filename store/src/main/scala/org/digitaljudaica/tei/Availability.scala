package org.digitaljudaica.tei

import org.digitaljudaica.xml.{Descriptor, Xml}
import scala.xml.{Elem, Node}

final case class Availability(
  status: Option[String],
  xml: Seq[Node]
)

object Availability extends Descriptor[Availability](
  elementName = "availability",
  contentParser = for {
    status <- Xml.attribute.optional("status")
    xml <- Xml.allNodes
  } yield new Availability(
    status,
    xml
  )
) {
  def apply(status: String, xml: Seq[Node]): Availability = new Availability(
    status = Some(status),
    xml
  )

  override def toXml(value: Availability): Elem =
    <availability status={value.status.orNull}>{value.xml}</availability>
}
