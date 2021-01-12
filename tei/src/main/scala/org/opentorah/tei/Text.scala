package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, Element, Parsable, Parser, Xml}

final case class Text(
  lang: Option[String],
  body: Body.Value
)

object Text extends Element[Text]("text") {

  private val langAttribute: Attribute.Optional[String] = Xml.langAttribute.optional

  override def contentParsable: Parsable[Text] = new Parsable[Text] {
    override val parser: Parser[Text] = for {
      lang <- langAttribute()
      body <- Body.element.required()
    } yield new Text(
      lang,
      body
    )

    override val unparser: Unparser[Text] = Tei.concat(
      langAttribute(_.lang),
      Body.element.required(_.body)
    )
  }
}
