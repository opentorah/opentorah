package org.opentorah.tei

import org.opentorah.xml.{Unparser, Attribute, ContentType, Element, Parsable, Parser, Xml}

final case class Ref(
  target: String,
  text: Seq[Xml.Node]
)

object Ref extends Element[Ref]("ref") {

  private val targetAttribute: Attribute.Required[String] = Attribute("target").required

  override def contentType: ContentType = ContentType.Mixed

  override def contentParsable: Parsable[Ref] = new Parsable[Ref] {
    override val parser: Parser[Ref] = for {
      target <- targetAttribute()
      text <- Element.nodes()
    } yield new Ref(
      target,
      text
    )

    override val unparser: Unparser[Ref] = Tei.concat(
      targetAttribute(_.target),
      Element.nodes(_.text)
    )
  }
}
