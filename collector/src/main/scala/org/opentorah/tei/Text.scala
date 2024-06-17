package org.opentorah.tei

import org.opentorah.xml.{Attribute, ElementTo, Parsable, Parser, Unparser, Xml}

final class Text(
  val lang: Option[String],
  val body: Body.Value
)

object Text extends ElementTo[Text]("text"):

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
