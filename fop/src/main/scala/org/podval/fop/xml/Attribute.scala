package org.podval.fop.xml

import org.w3c.dom.{Document, Element}
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

// Type-safe XML attribute get/set - for use in DOM and SAX.
// Inspired by net.sourceforge.jeuclid.context.Parameter and friends.
trait Attribute[T] {
  def namespace: Namespace

  def name: String

  final def qName: String = namespace.qName(name)

  def fromString(value: String): T

  def toString(value: T): String

  final def get(document: Document): Option[T] = {
    val element: Element = document.getDocumentElement
    get(element.getAttributeNS(namespace.uri, name))
  }

  final def doGet(document: Document): T = get(document).get

  final def getWithDefault(document: Document): T = get(document).getOrElse(default)

  final def set(value: T, document: Document): Unit = {
    val element = document.getDocumentElement
    val inDefaultNamespace: Boolean = namespace.is(element.getNamespaceURI)
    if (inDefaultNamespace) element.setAttribute(name, toString(value))
    else {
      // declare the attribute's namespace if it is not declared
      namespace.ensureDeclared(element)

      element.setAttributeNS(namespace.uri, qName, toString(value))
    }
  }

  final def get(defaultNamespace: Namespace, attributes: Attributes): Option[T] = {
    // Note: when attribute is in the default namespace, it need to be retrieved accordingly.
    // This is needed for the 'display' attribute (the only attribute this method is used for) of the included MathML -
    // but somehow does not seem to break the inline MathML either :)
    val inDefaultNamespace: Boolean = namespace.is(defaultNamespace)

    get(attributes.getValue(
      if (inDefaultNamespace) "" else namespace.uri,
      name
    ))
  }

  final def set(defaultNamespace: Namespace, value: T, attributes: AttributesImpl): Unit = {
    // Note: when attribute is added with the default namespace, this is not detected and a new namespace
    // with the same URI gets auto-declared - is this a bug or a feature?
    // Work-around: set the attribute *without* the namespace when I know that it is the default one!
    val inDefaultNamespace: Boolean = namespace.is(defaultNamespace)

    /* Note: if namespace.ensureDeclared() *is* called, I get error parsing the resulting fo:
      org.xml.sax.SAXParseException
      The prefix "xmlns" cannot be bound to any namespace explicitly; neither can the namespace for "xmlns" be bound to any prefix explicitly.
          at org.apache.xerces.parsers.AbstractSAXParser.parse(Unknown Source)
          at org.apache.xerces.jaxp.SAXParserImpl$JAXPSAXParser.parse(Unknown Source)
          at com.icl.saxon.IdentityTransformer.transform(IdentityTransformer.java:59)
      If it is not - there is still somehow "mathjax" namespace declaration in the output of the MathReader...

      if (!inDefaultNamespace) namespace.ensureDeclared(attributes)
     */

    attributes.addAttribute(
      if (inDefaultNamespace) "" else namespace.uri,
      name,
      if (inDefaultNamespace) name else qName,
      "CDATA",
      toString(value)
    )
  }

  def setWithDefault(defaultNamespace: Namespace, value: Option[T], attributes: AttributesImpl): Unit =
    set(defaultNamespace, value.getOrElse(default), attributes)

  def default: T

  private def get(value: String): Option[T] =
    Option(value).filter(_.nonEmpty).map(fromString)
}

object Attribute {

  trait BooleanAttribute extends Attribute[Boolean] {
    override def fromString(value: String): Boolean = value.toBoolean

    override def toString(value: Boolean): String = value.toString
  }

  trait StringAttribute extends Attribute[String] {
    override def fromString(value: String): String = value

    override def toString(value: String): String = value
  }

  trait FloatAttribute extends Attribute[Float] {
    override def fromString(value: String): Float = value.toFloat

    override def toString(value: Float): String = value.toString
  }

  trait IntAttribute extends Attribute[Int] {
    override def fromString(value: String): Int = value.toInt

    override def toString(value: Int): String = value.toString
  }

  trait StringListAttribute extends Attribute[List[String]] {
    override def fromString(value: String): List[String] = value.split(",").toList.map(_.trim)

    override def toString(value: List[String]): String = value.mkString(",")
  }
}
