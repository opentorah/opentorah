package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}
import scala.xml.Elem

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Descriptor[Language](
  elementName = "language",
  contentType = ContentType.Mixed,
  contentParser = for {
    ident <- Xml.attribute.required("ident")
    usage <- Xml.attribute.optional.positiveInt("usage")
    text <- Xml.text.optional
  } yield new Language(
    ident,
    usage,
    text
  )
) {
  override def toXml(value: Language): Elem =
    <language ident={value.ident} usage={value.usage.map(_.toString).orNull}>{value.text.orNull}</language>
}
