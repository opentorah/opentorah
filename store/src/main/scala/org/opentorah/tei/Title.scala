package org.opentorah.tei

import org.opentorah.xml.{ContentType, Descriptor, Xml}
import scala.xml.{Elem, Node}

final case class Title(
  titleType: Option[String],
  content: Seq[Node]
)

object Title extends Descriptor[Title](
  elementName = "title",
  contentType = ContentType.Mixed,
  contentParser = for {
    titleType <- Xml.attribute.optional("type")
    content <- Xml.allNodes
  } yield new Title(
    titleType,
    content
  )
) {
  override def toXml(value: Title): Elem =
    <title type={value.titleType.orNull}>{value.content}</title>
}
