package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser, Xml}

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element[Text]("text") {

  override val parser: Parser[Text] = for {
    lang <- Xml.langAttribute.optional
    body <- Body.parsable.required
  } yield new Text(
    lang,
    body
  )

  override val antiparser: Antiparser[Text] = Tei.concat(
    Xml.langAttribute.toXmlOption(_.lang),
    Body.parsable.toXml(_.body)
  )
}
