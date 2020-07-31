package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ContentType, Element, Parser, Xml}
import scala.xml.{Elem, Node}

final case class Ref(
  target: String,
  rendition: Option[String],
  text: Seq[Node]
)

object Ref extends Element.WithToXml[Ref]("ref") {

  val targetAttribute: Attribute[String] = Attribute("target")
  val renditionAttribute: Attribute[String] = Attribute("rendition")

  override protected def contentType: ContentType = ContentType.Mixed

  override protected def parser: Parser[Ref] = for {
    target <- targetAttribute.required
    rendition <- renditionAttribute.optional
    text <- Element.allNodes
  } yield new Ref(
    target,
    rendition,
    text
  )

  override protected def attributes(value: Ref): Seq[Attribute.Value[_]] = Seq(
    targetAttribute.withValue(value.target),
    renditionAttribute.withValue(value.rendition)
  )

  override protected def content(value: Ref): Seq[Node] =
    value.text

  def toXml(
    target: Seq[String],
    text: String,
    rendition: Option[String] = None
  ): Elem = toXml(new Ref(Files.mkUrl(target), rendition, Xml.mkText(text)))

  def toXml(
    target: Seq[String],
    text: Seq[Node]
  ): Elem = toXml(new Ref(Files.mkUrl(target), rendition = None, text))
}
