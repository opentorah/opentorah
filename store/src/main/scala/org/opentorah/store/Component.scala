package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Parser}
import zio.ZIO
import scala.xml.Elem

abstract class Component(elementName: String) {

  trait Element

  // Note: FromFile should be declared final, but if it is it triggers Scalac bug
  // https://github.com/scala/bug/issues/4440 and causes a warning
  // "The outer reference in this type test cannot be checked at run time".
  case class FromFile(file: String) extends Element

  trait WithClassName extends Element {
    def className: Option[String]
  }

  type Inline <: WithClassName

  def classOfInline: Class[_]

  object parsable extends org.opentorah.xml.Element.WithToXml[Element](elementName) {
    override protected def parser: Parser[Element] = for {
      file <- Component.fileAttribute.optional
      result <- if (file.isDefined) ZIO.succeed(FromFile(file.get)) else  for {
        className <- Attribute("type").optional
        result <- className.flatMap(delegate)
          .getOrElse(Component.this)
          .inlineParser(className)
          .map(_.asInstanceOf[Element])
      } yield result
    } yield result

    override protected def attributes(value: Element): Seq[Attribute.Value[_]] = value match {
      case FromFile(file) => Seq(Component.fileAttribute.withValue(file))
      case inline => inlineAttributes(inline.asInstanceOf[Inline])
    }

    override protected def content(value: Element): Seq[Elem] = value match {
      case FromFile(_) => Seq.empty
      case inline => inlineContent(inline.asInstanceOf[Inline])
    }
  }

  private def delegate(className: String): Option[Component] = None

  def inlineParser(className: Option[String]): Parser[Inline]

  protected def inlineAttributes(value: Inline): Seq[Attribute.Value[_]]

  protected def inlineContent(value: Inline): Seq[Elem]

  final type Creator[+R] = (
    /* inheritedSelectors: */ Seq[Selector],
    /* urls: */ Urls,
    /* inline: */ Inline) => R

  private [store] final def read[R](
    fromUrl: URL,
    inheritedSelectors: Seq[Selector] = Selector.predefinedSelectors,
    creator: Creator[R]
  ): R = fromElement[R](
    inheritedSelectors,
    urls = Urls.fromUrl(fromUrl),
    element = Parser.parseDo(parsable.parse(fromUrl)),
    creator
  )

  final def fromElement[R](
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    element: Element,
    creator: Creator[R]
  ): R = element match {

    case FromFile(file) => read[R](
      fromUrl = Files.fileInDirectory(urls.baseUrl, file),
      inheritedSelectors,
      creator
    )

    case element =>
      val inline = element.asInstanceOf[Inline]
      inline.className.map(reflectedCreator[R]).getOrElse(creator)(
        inheritedSelectors,
        urls,
        inline
      )
  }

  private def reflectedCreator[R](className: String): Creator[R] = {
    val constructor = Class.forName(className).getConstructor(
      classOf[Seq[Selector]],
      classOf[Urls],
      classOfInline
    )

    (
      inheritedSelectors: Seq[Selector],
      urls: Urls,
      inline: Inline
    ) => constructor.newInstance(
      inheritedSelectors,
      urls,
      inline
    ).asInstanceOf[R]
  }
}

object Component {
  val fileAttribute: Attribute[String] = Attribute("file")

  val typeAttribute: Attribute[String] = Attribute("type")
}
