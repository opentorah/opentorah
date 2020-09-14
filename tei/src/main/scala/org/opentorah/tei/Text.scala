package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser, Xml}

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element.WithToXml[Text]("text") {

  override protected val parser: Parser[Text] = for {
    lang <- Xml.langAttribute.optional
    body <- Body.parsable.required
  } yield new Text(
    lang,
    body
  )

  override protected val antiparser: Antiparser[Text] = Tei.concat(
    Xml.langAttribute.toXmlOption.compose[Text](_.lang),
    Body.parsable.toXml.compose[Text](_.body)
  )
}
