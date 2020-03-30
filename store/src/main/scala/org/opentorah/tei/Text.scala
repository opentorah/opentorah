package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, ToXml}
import scala.xml.Elem

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element[Text](
  elementName = "text",
  contentType = ContentType.Elements,
  parser = for {
    lang <- Attribute("xml:lang").optional
    body <- Body.parsable.required
  } yield new Text(
    lang,
    body
  )
) with ToXml[Text] {

  override def toXml(value: Text): Elem =
    <text xml:lang={value.lang.orNull}>{Body.parsable.toXml(value.body)}</text>
}
