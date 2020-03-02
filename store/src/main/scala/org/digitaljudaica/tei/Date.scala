package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}
import scala.xml.Node

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
  ),
  toXml = (value: Date) => <date when={value.when} calendar={value.calendar.orNull}>{value.xml}</date>
)
