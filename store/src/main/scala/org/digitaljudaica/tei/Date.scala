package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}
import scala.xml.{Elem, Node}

final case class Date(
  when: String,
  calendar: Option[String],
  xml: Seq[Node]
)

object Date extends Descriptor[Date](
  elementName = "date",
  contentType = ContentType.Mixed,
  contentParser = for {
    when <- Xml.attribute.required("when")
    calendar <- Xml.attribute.optional("calendar")
    xml <- Xml.allNodes
  } yield new Date(
    when,
    calendar,
    xml
  )
) {
  override def toXml(value: Date): Elem =
    <date when={value.when} calendar={value.calendar.orNull}>{value.xml}</date>
}
