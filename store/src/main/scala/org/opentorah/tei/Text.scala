package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Descriptor}
import scala.xml.{Elem, Node}

final case class Text(
  lang: Option[String],
  body: Body
)

object Text extends Descriptor[Text](
  elementName = "text",
  contentType = ContentType.Elements,
  contentParser = for {
    lang <- Attribute("xml:lang").optional
    body <- Body.required
  } yield new Text(
    lang,
    body
  )
) {

  def apply(xml: Seq[Node]): Text = new Text(
    lang = None,
    body = Body(xml)
  )

  override def toXml(value: Text): Elem =
    <text xml:lang={value.lang.orNull}>{Body.toXml(value.body)}</text>
}
