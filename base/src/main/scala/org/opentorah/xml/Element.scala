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

  final override def xmlElement(value: A): ScalaXml.Element =
    ScalaXml.setAttributes(
      attributes = contentParsable.unparser.attributes(value),
      element = contentParsable.unparser.namespace.fold(<elem/>)(namespace => ScalaXml.declareNamespace(namespace.default, <elem/>))
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

  // TODO move to the top level?
  final class Nodes(val xml: Xml)(val nodes: xml.Nodes) {
    // TODO: if xml != ScalaXml, this will fail *at run-time*...
    def scalaXml: ScalaXml.Nodes = nodes.asInstanceOf[ScalaXml.Nodes]
    
    override def toString: String = xml.toString(nodes)
  }

  val nodes: Parsable[Nodes] = new Parsable[Nodes]:
    override protected def parser: Parser[Nodes] = Parsing.allNodes
    override def unparser: Unparser[Nodes] = Unparser[Nodes](content = _.scalaXml)
    
  final class FromUrl(
    val url: URL,
    val inline: Boolean
  )

  object FromUrl:
    trait With:
      def fromUrl: FromUrl

  def fromUrl: Parser[FromUrl] = Parsing.fromUrl

//  abstract class WithToXmlFromUrl[A <: FromUrl.With](elementName: String) extends Element[A](elementName) {
//    final def withRedirect(fromUrl: FromUrl.With, follow: Boolean): Elements[A] = new Elements[A] {
//      override def toXmlElement(value: A): Xml.Element =
//        if (follow || value.fromUrl.inline) WithToXmlFromUrl.this.toXmlElement(value)
//        else constructRedirect(relativize(value.fromUrl.url, fromUrl.fromUrl.url).toString)
//
//      override private[xml] def canParse(elementName: String) = ???
//      override protected def optionalParser: Parser[Option[A]] = ??? // TODO
//    }
//
//    private def relativize(url: URL, base: URL): URL =
//      url.toURI.relativize(base.toURI).toURL
//
//    private def constructRedirect(redirect: String): Xml.Element = Xml.construct(
//      name = elementName,
//      namespace = None,
//      attributes = Seq(redirectAttribute.required.withValue(redirect)),
//      children = Seq.empty
//    )
//  }
