package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parser, Xml}
import scala.xml.{Elem, Node}

final case class Ref(
  target: String,
  rendition: Option[String],
  text: Seq[Node]
)

object Ref extends Element.WithToXml[Ref]("ref") {

  val roleAttribute: Attribute[String] = Attribute("role")
  val targetAttribute: Attribute[String] = Attribute("target")
  val renditionAttribute: Attribute[String] = Attribute("rendition")

  override protected def contentType: ContentType = ContentType.Mixed

  override protected val parser: Parser[Ref] = for {
    target <- targetAttribute.required
    rendition <- renditionAttribute.optional
    text <- Element.allNodes
  } yield new Ref(
    target,
    rendition,
    text
  )

  override protected val antiparser: Antiparser[Ref] = Tei.concat(
    targetAttribute.toXml.compose(_.target),
    renditionAttribute.toXmlOption.compose(_.rendition),
    Antiparser.xml.compose(_.text)
  )

  def toXml(
    target: Seq[String],
    text: String,
    rendition: Option[String] = None
  ): Elem = toXmlElement(new Ref(Files.mkUrl(target), rendition, Xml.mkText(text)))

  def toXml(
    target: Seq[String],
    text: Seq[Node]
  ): Elem = toXmlElement(new Ref(Files.mkUrl(target), rendition = None, text))

  def transformer(resolver: TeiResolver): Xml.Transformer = elem => if (elem.label != elementName) elem else {
    if (elem.child.forall(Xml.isWhitespace)) println(s"No reference text: $elem")
    targetAttribute.get(elem).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
      if (!target.startsWith("/")) elem else {
        val (url, part) = Files.urlAndPart(target)
        resolver.resolve(url).fold(elem) { resolved =>
          val role: Option[String] = resolved.role
          if (role.isEmpty) elem else {
            Attribute.setAll(elem, Seq(
              roleAttribute.withValue(role),
              targetAttribute.withValue(Files.mkUrl(Files.addPart(resolved.url, part))),
              renditionAttribute.withValue(renditionAttribute.get(elem))
            ))
          }
        }
      }
    }
  }
}
