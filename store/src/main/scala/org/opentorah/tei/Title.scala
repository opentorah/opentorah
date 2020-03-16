package org.opentorah.tei

import org.opentorah.xml.{Attribute, ContentType, Element, Parser, ToXml}
import scala.xml.{Elem, Node}

final case class Title(
  titleType: Option[String],
  content: Seq[Node]
)

object Title extends Element[Title](
  elementName = "title",
  contentType = ContentType.Mixed,
  parser = for {
    titleType <- Attribute("type").optional
    content <- Parser.allNodes
  } yield new Title(
    titleType,
    content
  )
) with ToXml[Title] {
  override def toXml(value: Title): Elem =
    <title type={value.titleType.orNull}>{value.content}</title>
}
