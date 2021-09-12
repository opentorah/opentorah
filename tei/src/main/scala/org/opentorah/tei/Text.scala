package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, Element, Parsable, Parser, Xml}

final class Text(
  val lang: Option[String],
  val body: Body.Value
)

object Text extends Element[Text]("text"):

  private val langAttribute: Attribute.Optional[String] = Xml.langAttribute.optional

  override def contentParsable: Parsable[Text] = new Parsable[Text]:
    override val parser: Parser[Text] = for
      lang: Option[String] <- langAttribute()
      body: Body.Value <- Body.element.required()
    yield Text(
      lang,
      body
    )

    override val unparser: Unparser[Text] = Tei.concat(
      langAttribute(_.lang),
      Body.element.required(_.body)
    )
