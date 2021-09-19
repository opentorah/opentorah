package org.opentorah.xml

abstract class Element[A](val elementName: String) extends Elements[A]:

  override def toString: String = s"element '$elementName' [$contentType]"

  def contentType: ContentType = ContentType.Elements

  def contentParsable: Parsable[A]

  final override protected def elementByName(elementName: String): Option[Element[A]] =
    if this.elementName != elementName then None else Some(this)

  override protected def elementByValue(value: A): Element[?] = this

  final override protected def mapParser(element: Element[?], parser: Parser[?]): Parser[A] =
    parser.asInstanceOf[Parser[A]]

  override def xmlElement(value: A): ScalaXml.Element =
    ScalaXml.setAttributes(
      contentParsable.unparser.attributes(value),
      contentParsable.unparser.namespace.fold(<elem/>)(namespace => ScalaXml.declareNamespace(namespace.default, <elem/>))
    ).copy(
      label = elementName,
      child = contentParsable.unparser.content(value)
    )

object Element:
  def currentFromUrl: Parser[FromUrl] = Parsing.currentFromUrl

  val nodes: Parsable[ScalaXml.Nodes] = new Parsable[ScalaXml.Nodes]:
    override protected def parser: Parser[ScalaXml.Nodes] = Parsing.allNodes
    override def unparser: Unparser[ScalaXml.Nodes] = Unparser[ScalaXml.Nodes](content = identity)

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
