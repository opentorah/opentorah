package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element}
import scala.xml.Elem

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Element[Language](
  elementName = "language",
  contentType = ContentType.Mixed,
  parser = for {
    ident <- Attribute("ident").required
    usage <- Attribute("usage").positiveInt.optional
    text <- org.opentorah.xml.Text().optional
  } yield new Language(
    ident,
    usage,
    text
  )
) {
  override def toXml(value: Language): Elem =
    <language ident={value.ident} usage={value.usage.map(_.toString).orNull}>{value.text.orNull}</language>
}
