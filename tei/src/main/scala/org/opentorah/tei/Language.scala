package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Element.WithToXml[Language]("language") {

  private val identAttribute: Attribute[String] = Attribute("ident")
  private val usageAttribute: Attribute.PositiveIntAttribute = Attribute.PositiveIntAttribute("usage")

  override protected def contentType: ContentType = ContentType.Mixed

  override protected def parser: Parser[Language] = for {
    ident <- identAttribute.required
    usage <- usageAttribute.optional
    text <- org.opentorah.xml.Text().optional
  } yield new Language(
    ident,
    usage,
    text
  )

  override protected val antiparser: Antiparser[Language] = Antiparser(
    identAttribute.toXml.compose[Language](_.ident),
    usageAttribute.toXmlOption.compose[Language](_.usage),
    Antiparser.xml.compose[Language](value => value.text.toSeq.map(Xml.mkText))
  )
}
