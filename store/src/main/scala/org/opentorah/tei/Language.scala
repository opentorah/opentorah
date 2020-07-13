package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, Parser, Xml}
import scala.xml.Node

final case class Language(
  ident: String,
  usage: Option[Int],
  text: Option[String]
)

object Language extends Element.WithToXml[Language]("language") {

  private val identAttribute: Attribute[String] = Attribute("ident")
  private val usageAttribute: Attribute.PositiveIntAttribute = Attribute.positiveInt("usage")

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

  override protected def attributes(value: Language): Seq[Attribute.Value[_]] = Seq(
    identAttribute.withValue(value.ident),
    usageAttribute.withValue(value.usage)
  )

  override protected def content(value: Language): Seq[Node] =
    value.text.toSeq.map(Xml.textNode)
}
