package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element.WithToXml[Text]("text") {

  private val xmlLangAttribute: Attribute[String] = Attribute("xml:lang")

  override protected def parser: Parser[Text] = for {
    lang <- xmlLangAttribute.optional
    body <- Body.parsable.required
  } yield new Text(
    lang,
    body
  )

  override protected def attributes(value: Text): Seq[Attribute.Value[_]] = Seq(
    xmlLangAttribute.withValue(value.lang)
  )

  override protected def content(value: Text): Seq[Elem] =
    Seq(Body.parsable.toXml(value.body))
}
