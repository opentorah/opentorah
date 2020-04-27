package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ContentType, Element, ToXml, Xml}
import scala.xml.{Elem, Node}

final case class Ref(
  target: String,
  rendition: Option[String],
  text: Seq[Node]
)

object Ref extends Element[Ref](
  elementName = "ref",
  contentType = ContentType.Mixed,
  parser = for {
    target <- Attribute("target").required
    rendition <- Attribute("rendition").optional
    text <- Element.allNodes
  } yield new Ref(
    target,
    rendition,
    text
  )
) with ToXml[Ref] {

  override def toXml(value: Ref): Elem =
    <ref target={value.target} rendition={value.rendition.orNull}>{value.text}</ref>


  def toXml(
    target: Seq[String],
    text: String,
    rendition: Option[String] = None
  ): Elem = toXml(new Ref(Files.mkUrl(target), rendition, Xml.textNode(text)))

  def toXml(
    target: Seq[String],
    text: Seq[Node]
  ): Elem = toXml(new Ref(Files.mkUrl(target), rendition = None, text))
}
