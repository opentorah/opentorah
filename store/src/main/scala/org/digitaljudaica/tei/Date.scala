package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}

final case class Date(
  when: String,
  calendar: Option[String],
  text: Option[String]
)

object Date extends Descriptor[Date](
  elementName = "date",
  contentType = ContentType.Mixed,
  contentParser = for {
    when <- Xml.attribute.required("when")
    calendar <- Xml.attribute.optional("calendar")
    text <- Xml.text.optional
  } yield new Date(
    when,
    calendar,
    text
  )
)
