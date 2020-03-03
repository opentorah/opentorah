package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}
import scala.xml.Node

final case class Text(
  lang: Option[String],
  body: Body
)

object Text extends Descriptor[Text](
  elementName = "text",
  contentType = ContentType.Elements,
  contentParser = for {
    lang <- Xml.attribute.optional("xml:lang")
    body <- Body.required
  } yield new Text(
    lang,
    body
  ),
  toXml = (value: Text) => <text xml:lang={value.lang.orNull}>{Body.toXml(value.body)}</text>
) {

  def apply(xml: Seq[Node]): Text = new Text(
    lang = None,
    body = Body(xml)
  )
}
