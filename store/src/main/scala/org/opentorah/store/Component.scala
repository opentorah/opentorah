package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Parser, ToXml}
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

  object parsable extends org.opentorah.xml.Element[Element](elementName, parser = for {
    file <- Attribute("file").optional
    result <- if (file.isDefined) ZIO.succeed(FromFile(file.get)) else  for {
      className <- Attribute("type").optional
      result <- inlineParser(className)
    } yield result
  } yield result) with ToXml[Element] {

    override def toXml(value: Element): Elem = value match {
      case FromFile(file) => <element file={file}/>.copy(label = elementName)
      case inline => inlineToXml(inline.asInstanceOf[Inline])
    }
  }

  def inlineParser(className: Option[String]): Parser[Inline]

  // TODO with some generalization of Attributes it should be possible to factor parts of inlineToXml() into here:
  def inlineToXml(value: Inline): Elem

  final type Creator[+R] = (
    /* inheritedSelectors: */ Seq[Selector],
    /* fromUrl: */ Option[URL],
    /* baseUrl: */ URL,
    /* inline: */ Inline) => R

  final def read[R](
    fromUrl: URL,
    inheritedSelectors: Seq[Selector] = Selector.predefinedSelectors,
    creator: Creator[R]
  ): R = fromElement[R](
    inheritedSelectors,
    fromUrl = Some(fromUrl),
    baseUrl = fromUrl,
    element = Parser.parseDo(parsable.parse(fromUrl)),
    creator
  )

  final def fromElement[R](
    inheritedSelectors: Seq[Selector],
    fromUrl: Option[URL],
    baseUrl: URL,
    element: Element,
    creator: Creator[R]
  ): R = element match {

    case FromFile(file) => read[R](
      fromUrl = Files.fileInDirectory(baseUrl, file),
      inheritedSelectors,
      creator
    )

    case element =>
      val inline = element.asInstanceOf[Inline]
      inline.className match {

      case None => creator(
        inheritedSelectors,
        fromUrl,
        baseUrl,
        inline
      )

      case Some(className) => Class.forName(className)
        .getConstructor(
          classOf[Seq[Selector]],
          classOf[Option[URL]],
          classOf[URL],
          classOfInline
        )
        .newInstance(
          inheritedSelectors,
          fromUrl,
          baseUrl,
          element
        )
        .asInstanceOf[R]
    }
  }
}
