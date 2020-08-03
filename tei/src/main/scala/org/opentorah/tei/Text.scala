package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, Element, Parser}

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element.WithToXml[Text]("text") {

  private val xmlLangAttribute: Attribute[String] = Attribute("xml:lang")

  override protected val parser: Parser[Text] = for {
    lang <- xmlLangAttribute.optional
    body <- Body.parsable.required
  } yield new Text(
    lang,
    body
  )

  override protected val antiparser: Antiparser[Text] = Antiparser(
    xmlLangAttribute.toXmlOption.compose[Text](_.lang),
    Body.parsable.toXml.compose[Text](_.body)
  )
}
