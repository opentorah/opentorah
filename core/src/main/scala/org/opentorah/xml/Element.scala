package org.opentorah.xml

import java.net.URL

abstract class Element[A](val elementName: String) extends Elements[A]:

  override def toString: String = s"element '$elementName' [$contentType]"

  def contentType: Element.ContentType = Element.ContentType.Elements

  def contentParsable: Parsable[A]

  final override def elementAndParser(elementName: String): Option[Element.AndParser[A]] =
    if this.elementName != elementName then None else Some(Element.AndParser(
      element = this,
      parser = contentParsable()
    ))

  final override protected def elementByValue(value: A): Element[?] = this

  // TODO allow declaring this element's namespace...
  final override def xmlElement(value: A): Xml.Element =
    Attribute.set(
      attributes = contentParsable.unparser.attributes(value),
      element = contentParsable.unparser.namespace.fold(<elem/>)(namespace => namespace.default.declare(<elem/>))
    ).copy(
      label = elementName,
      child = contentParsable.unparser.content(value)
    )

object Element:

  enum ContentType(
    val elementsAllowed: Boolean,
    val charactersAlowed: Boolean
  ) derives CanEqual:
    case Empty      extends ContentType(elementsAllowed = false, charactersAlowed = false)
    case Characters extends ContentType(elementsAllowed = false, charactersAlowed = true )
    case Elements   extends ContentType(elementsAllowed = true , charactersAlowed = false)
    case Mixed      extends ContentType(elementsAllowed = true , charactersAlowed = true )

  final class AndParser[A](val element: Element[?], val parser: Parser[A])

  // TODO remove
  final class Nodes(val nodes: Xml.Nodes):
    override def toString: String = Xml.toString(nodes)

  val nodes: Parsable[Nodes] = new Parsable[Nodes]:
    override protected def parser: Parser[Nodes] = Parsing.allNodes
    override def unparser: Unparser[Nodes] = Unparser[Nodes](content = _.nodes)

  final class FromUrl(
    val url: URL,
    val inline: Boolean
  )

  object FromUrl:
    trait With:
      def fromUrl: FromUrl

  def fromUrl: Parser[FromUrl] = Parsing.fromUrl
