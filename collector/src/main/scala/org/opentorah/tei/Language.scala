package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, ElementTo, Parsable, Parser, Unparser}

final class Language(
  val ident: String,
  val usage: Option[Int],
  val text: Option[String]
)

object Language extends ElementTo[Language]("language"):

  private val identAttribute: Attribute.Required[String] = Attribute("ident").required
  private val usageAttribute: Attribute.Optional[Int] = Attribute.PositiveIntAttribute("usage").optional
  private val textParsable: Parsable[Option[String]] = org.opentorah.xml.Text().optional

  override def contentType: ContentType = ContentType.Mixed

  override def contentParsable: Parsable[Language] = new Parsable[Language]:
    override def parser: Parser[Language] = for
      ident: String <- identAttribute()
      usage: Option[Int] <- usageAttribute()
      text: Option[String] <- textParsable()
    yield Language(
      ident,
      usage,
      text
    )

    override val unparser: Unparser[Language] = Tei.concat(
      identAttribute(_.ident),
      usageAttribute(_.usage),
      textParsable(_.text)
    )
