package org.opentorah.xml

import org.opentorah.util.{Files, Strings}
import org.xml.sax.helpers.XMLFilterImpl
import org.xml.sax.{InputSource, Locator, SAXParseException}
import java.io.{File, StringReader}
import java.net.URL

// Note: declareNamespace() and setAttribute() modify in-place.
object Sax extends XmlAttributes:

  trait WarningFilter extends XMLFilterImpl:
    private var locator: Option[Locator] = None

    protected def warning(message: String): Unit = getErrorHandler.warning(SAXParseException(message, locator.orNull))

    override def setDocumentLocator(locator: Locator): Unit =
      super.setDocumentLocator(locator)
      this.locator = Some(locator)

  def string2inputSource(string: String): InputSource = InputSource(StringReader(string))
  def file2inputSource(file: File): InputSource = url2inputSource(Files.file2url(file))
  def url2inputSource(url: URL): InputSource = InputSource(url.toString)

  override type Attributes = org.xml.sax.Attributes
  override type Element = org.xml.sax.helpers.AttributesImpl

  // TODO implement
  override def getNamespace(attributes: Attributes): Namespace = ???

  override def getNamespaces(attributes: Attributes): Seq[Namespace] =
    for
      index: Int <- 0 until attributes.getLength
      prefix: Option[String] = getPrefix(attributes, index)
      if prefix == Namespace.Xmlns.getPrefix
      uri: Option[String] = Strings.empty2none(attributes.getURI(index))
      localName: Option[String] = Strings.empty2none(attributes.getLocalName(index))
    yield
      // TODO why ||? How does it look for non-default namespaces?
      require(uri.isEmpty || (uri == Namespace.Xmlns.getUri))
      Namespace(
        prefix = localName,
        uri = Option(attributes.getValue(index))
      )

  override def isNamespaceDeclared(namespace: Namespace, attributes: Attributes): Boolean =
    namespace.attribute.getStringOption(Sax)(attributes) == namespace.getUri

  override def declareNamespace(namespace: Namespace, element: Element): Element =
    namespace.attributeValue.set(Sax)(element)

  /*
    Note: when attribute is in the default namespace, it needs to be retrieved accordingly; I use inNamespace() for that.
    This is needed for the 'display' attribute (the only attribute this method is used for) of the included MathML -
    but somehow does not seem to break the inline MathML either :)

    Note: we ignore attribute `type` (attributes.getType(index)) and assume "CNAME".
   */
  override def getAttribute(attribute: Attribute[?], attributes: Attributes): Option[String] =
    Option(attributes.getValue(
      attribute.namespace.getUri.getOrElse(""),
      attribute.name
    ))

  override def getAttributes(attributes: Attributes): Attribute.StringValues =
    for
      index: Int <- 0 until attributes.getLength
      prefix: Option[String] = getPrefix(attributes, index)
      if prefix != Namespace.Xmlns.getPrefix
    yield Attribute(
      name = attributes.getLocalName(index),
      namespace = Namespace(
        uri = Strings.empty2none(attributes.getURI(index)),
        prefix = prefix
      )
    ).optional.withValue(Option(attributes.getValue(index)))

  private def getPrefix(
    attributes: Attributes,
    index: Int
  ): Option[String] =
    // TODO if qName is empty - is there a requirement on localName?
    val localName: String = attributes.getLocalName(index)

    Strings.empty2none(attributes.getQName(index)) flatMap(qName =>
      val (prefix: Option[String], tail: String) = Strings.splitRight(qName, ':')
      require(prefix.isEmpty || (tail == Strings.empty2none(localName).get))
      prefix
    )

  /*
  Note: when attribute is added with the default namespace, this is not detected and a new namespace
  with the same URI gets auto-declared - is this a bug or a feature?
  So, I set the attribute *without* the namespace when I know that it is the default one - using inNamespace()!

  Note: if namespace.ensureDeclared() *is* called, I get error parsing the resulting fo:
  org.xml.sax.SAXParseException
  The prefix "xmlns" cannot be bound to any namespace explicitly; neither can the namespace for "xmlns" be bound to any prefix explicitly.
      at org.apache.xerces.parsers.AbstractSAXParser.parse(Unknown Source)
      at org.apache.xerces.jaxp.SAXParserImpl$JAXPSAXParser.parse(Unknown Source)
      at com.icl.saxon.IdentityTransformer.transform(IdentityTransformer.java:59)
  If it is not - there is still somehow "mathjax" namespace declaration in the output of the MathReader...

  if (!inDefaultNamespace) namespace.ensureDeclared(attributes)
  */
  override def setAttribute[T](attribute: Attribute.Value[T], element: Element): Element = attribute.valueEffective match
    case None => element
    case Some(value) =>
      val name: String = attribute.attribute.name
      val namespace: Namespace = attribute.attribute.namespace
      element.addAttribute(
        namespace.getUri.getOrElse(""),
        name,
        namespace.qName(name),
        "CDATA",
        value
      )

      element

  override def setAttributes(attributes: Attribute.Values, element: Element): Element = ??? // TODO implement

  // Note: XMLObj.setAttributes() sets namespace on an attribute only if it already saw
  // the declarations of that namespace, so I am making sure that they are there (and in the beginning);
  // even then, XMLObj.setAttributes() sets un-prefixed qualified name for namespaced attributes -
  // but somehow they are detected correctly in MathJax.typeset()...
  def sortAttributes(attributes: Attributes): Element =
    val nonXmlns: Attribute.StringValues = getAttributes(attributes)
    val usedNamespaces: Set[Namespace] = nonXmlns.map(_.attribute.namespace).toSet
    val declaredNamespaces: Set[Namespace] = getNamespaces(attributes).toSet

    val result: Element = org.xml.sax.helpers.AttributesImpl()
    for namespace: Namespace <- usedNamespaces -- declaredNamespaces do declareNamespace(namespace, result)
    for attributeValue: Attribute.Value[String] <- nonXmlns do attributeValue.set(Sax)(result)
    result
