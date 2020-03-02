package org.digitaljudaica.tei

import org.digitaljudaica.xml.{ContentType, Descriptor, Xml}
import scala.xml.Node

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
  ),
  toXml = (value: Title) => <title type={value.titleType.orNull}>{value.content}</title>
)