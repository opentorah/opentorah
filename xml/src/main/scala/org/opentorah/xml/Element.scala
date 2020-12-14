package org.opentorah.xml

import org.opentorah.util.Files
import java.net.URL

// TODO turn all subclasses of Element into Element.WithToXml and collapse the latter into the former;
// TODO derive Parsable from ToXml...
abstract class Element[A](
  val elementName: String
) extends Parsable[A] with ToParse[A] {

  override def toString: String =
    s"element '$elementName'" +
    s" [$contentType]" +
    (if (!canRedirect) "" else " (can redirect)")

  final override def canParse(elementName: String): Option[ToParse[A]] =
    if (elementName != this.elementName) None else Some(
      if (!canRedirect) this
      else Element.withRedirect(this).mustParse(elementName)
    )

  def canRedirect: Boolean = false

  def contentType: ContentType = ContentType.Elements

  def parser: Parser[A]
}

object Element {

  abstract class WithToXml[A](override val elementName: String) extends Element[A](elementName) with ToXml[A] {
    final override def toXmlElement(value: A): Xml.Element = Xml.construct(
      name = elementName,
      namespace = antiparser.namespace,
      attributes = antiparser.attributes(value),
      children = antiparser.content(value)
    )

    protected def antiparser: Antiparser[A]
  }

  val allNodes: Parser[Seq[Xml.Node]] =
    Context.allNodes

  private val redirectAttribute: Attribute[String] = Attribute("file")

  private def withRedirect[A](element: Element[A]): Element[A] = new Element[A](element.elementName) {
    override def contentType: ContentType = element.contentType

    override def parser: Parser[A] = for {
      url <- redirectAttribute.optional
      result <- url.fold(element.parser) { url: String =>
        for {
          currentFromUrl <- Context.currentFromUrl
          from <- Parser.effect[URL](Files.subUrl(currentFromUrl, url))
          result <- parse(From.url(from, isRedirect = true))
        } yield result
      }
    } yield result
  }
}
