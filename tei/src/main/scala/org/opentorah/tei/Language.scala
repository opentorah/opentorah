package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Element[Language]("language") {

  private val identAttribute: Attribute[String] = Attribute("ident")
  private val usageAttribute: Attribute.PositiveIntAttribute = new Attribute.PositiveIntAttribute("usage")

  override def contentType: ContentType = ContentType.Mixed

  override def parser: Parser[Language] = for {
    ident <- identAttribute.required
    usage <- usageAttribute.optional
    text <- org.opentorah.xml.Text().optional
  } yield new Language(
    ident,
    usage,
    text
  )

  override val antiparser: Antiparser[Language] = Tei.concat(
    identAttribute.toXml(_.ident),
    usageAttribute.toXmlOption(_.usage),
    Antiparser.xml(value => value.text.toSeq.map(Xml.mkText)) // TODO xml2string?
  )
}
