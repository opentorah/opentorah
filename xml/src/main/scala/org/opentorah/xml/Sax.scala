package org.opentorah.xml

import org.opentorah.util.Strings
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

// Note: declareNamespace() and setAttribute() modify in-place.
object Sax {

  def getNamespaces(attributes: Attributes): Seq[Namespace] =
    for {
      index <- 0 until attributes.getLength
      prefix = getPrefix(attributes, index)
      if prefix == Namespace.Xmlns.getPrefix
      uri = Strings.empty2none(attributes.getURI(index))
      localName = Strings.empty2none(attributes.getLocalName(index))
    } yield {
      // TODO why ||? How does it look for non-default namespaces?
      require(uri.isEmpty || (uri == Namespace.Xmlns.getUri))
      Namespace(
        prefix = localName,
        uri = Option(attributes.getValue(index))
      )
    }

  def isNamespaceDeclared(namespace: Namespace, attributes: AttributesImpl): Boolean =
    namespace.attribute.get(attributes) == namespace.getUri

  def declareNamespace(namespace: Namespace, attributes: AttributesImpl): Unit =
    namespace.attributeValue.set(attributes)

  /*
    Note: when attribute is in the default namespace, it needs to be retrieved accordingly; I use inNamespace() for that.
    This is needed for the 'display' attribute (the only attribute this method is used for) of the included MathML -
    but somehow does not seem to break the inline MathML either :)

    Note: we ignore attribute `type` (attributes.getType(index)) and assume "CNAME".
   */
  def getAttribute(attribute: Attribute[_], attributes: Attributes): Option[String] =
    Option(attributes.getValue(
      attribute.namespace.getUri.getOrElse(""),
      attribute.name
    ))

  def getAttributes(attributes: Attributes): Seq[Attribute.Value[String]] =
    for {
      index <- 0 until attributes.getLength
      prefix = getPrefix(attributes, index)
      if prefix != Namespace.Xmlns.getPrefix
    } yield Attribute(
      name = attributes.getLocalName(index),
      namespace = Namespace(
        uri = Strings.empty2none(attributes.getURI(index)),
        prefix = prefix
      )
    ).withValue(Option(attributes.getValue(index)))

  private def getPrefix(
    attributes: Attributes,
    index: Int
  ): Option[String] = {
    // TODO if qName is empty - is there a requirement on localName?
    val localName: String = attributes.getLocalName(index)

    Strings.empty2none(attributes.getQName(index)) flatMap { qName =>
      val (prefix: Option[String], tail: String) = Strings.splitRight(qName, ':')
      require(prefix.isEmpty || (tail == Strings.empty2none(localName).get))
      prefix
    }
  }

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
  def setAttribute[T](attribute: Attribute[T], value: T, attributes: AttributesImpl): Unit = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    val valueStr: String = attribute.toString(value)
    attributes.addAttribute(
      namespace.getUri.getOrElse(""),
      name,
      namespace.qName(name),
      "CDATA",
      valueStr
    )
  }

  // Note: XMLObj.setAttributes() sets namespace on an attribute only if it already saw
  // the declarations of that namespace, so I am making sure that they are there (and in the beginning);
  // even then, XMLObj.setAttributes() sets un-prefixed qualified name for namespaced attributes -
  // but somehow they are detected correctly in MathJax.typeset()...
  def sortAttributes(attributes: Attributes): AttributesImpl = {
    val nonXmlns: Seq[Attribute.Value[String]] = getAttributes(attributes)
    val usedNamespaces: Set[Namespace] = nonXmlns.map(_.attribute.namespace).toSet
    val declaredNamespaces: Set[Namespace] = getNamespaces(attributes).toSet

    val result: AttributesImpl = new AttributesImpl
    for (namespace: Namespace <- usedNamespaces -- declaredNamespaces) namespace.declare(result)
    for (attributeValue: Attribute.Value[String] <- nonXmlns) attributeValue.set(result)
    result
  }
}
