package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}
import scala.xml.{Elem, Node}

final case class Ref(
  target: String,
  text: Seq[Node]
)

object Ref extends Element.WithToXml[Ref]("ref") {

  private val targetAttribute: Attribute[String] = Attribute("target")

  override def contentType: ContentType = ContentType.Mixed

  override val parser: Parser[Ref] = for {
    target <- targetAttribute.required
    text <- Element.allNodes
  } yield new Ref(
    target,
    text
  )

  override protected val antiparser: Antiparser[Ref] = Tei.concat(
    targetAttribute.toXml.compose(_.target),
    Antiparser.xml.compose(_.text)
  )

  def toXml(
    target: Seq[String],
    text: String
  ): Elem = toXmlElement(new Ref(Files.mkUrl(target), Xml.mkText(text)))

  def toXml(
    target: Seq[String],
    text: Seq[Node]
  ): Elem = toXmlElement(new Ref(Files.mkUrl(target), text))
}
