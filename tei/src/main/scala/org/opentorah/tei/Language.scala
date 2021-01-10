package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser}

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Element[Language]("language") {

  private val identAttribute: Attribute.Required[String] = Attribute("ident").required
  private val usageAttribute: Attribute.Optional[Int] = new Attribute.PositiveIntAttribute("usage").optional
  private val textParsable: Parsable[Option[String]] = org.opentorah.xml.Text().optional

  override def contentType: ContentType = ContentType.Mixed

  override def contentParsable: Parsable[Language] = new Parsable[Language] {
    override def parser: Parser[Language] = for {
      ident <- identAttribute()
      usage <- usageAttribute()
      text <- textParsable()
    } yield new Language(
      ident,
      usage,
      text
    )

    override val antiparser: Antiparser[Language] = Tei.concat(
      identAttribute(_.ident),
      usageAttribute(_.usage),
      textParsable(_.text)
    )
  }
}
