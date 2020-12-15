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
    identAttribute.toXml.compose(_.ident),
    usageAttribute.toXmlOption.compose(_.usage),
    Antiparser.xml.compose(value => value.text.toSeq.map(Xml.mkText))
  )
}
