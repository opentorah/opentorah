package org.opentorah.xml

import org.opentorah.util.Strings

/* from https://www.w3.org/TR/xml-names/

  The prefix xml is by definition bound to the namespace name http://www.w3.org/XML/1998/namespace.
  It MAY, but need not, be declared, and MUST NOT be bound to any other namespace name.
  Other prefixes MUST NOT be bound to this namespace name, and it MUST NOT be declared as the default namespace.

  The prefix xmlns is used only to declare namespace bindings and is by definition bound to the namespace
  name http://www.w3.org/2000/xmlns/. It MUST NOT be declared. Other prefixes MUST NOT be bound to this namespace name,
  and it MUST NOT be declared as the default namespace. Element names MUST NOT have the prefix xmlns.

  All other prefixes beginning with the three-letter sequence x, m, l, in any case combination, are reserved.
  This means that:
    users SHOULD NOT use them except as defined by later specifications
    processors MUST NOT treat them as fatal errors.

  If there is no default namespace declaration in scope, the namespace name has no value.
  The namespace name for an unprefixed attribute name always has no value.
*/
sealed trait Namespace {

  def getUri: Option[String]

  def uri: String

  def getPrefix: Option[String]

  def qName(localName: String): String

  override def toString: String = attributeValue.toString

  override def equals(other: Any): Boolean = other match {
    case that: Namespace => (this.getUri == that.getUri) && (this.getPrefix == that.getPrefix)
    case _ => false
  }

  final def isDefault: Boolean = getPrefix.isEmpty

  final def default: Namespace = if (isDefault) this else Namespace(prefix = None, uri = getUri)

  // Note: empty string attribute name is used for default namespace attributes;
  // it is processed specially by Namespace.Xmlns.qName()
  final def attribute: Attribute[String] =
    new Attribute.StringAttribute(getPrefix.getOrElse(""), Namespace.Xmlns)

  final def attributeValue: Attribute.Value[String] = attribute.withValue(getUri)

  // Scala XML
  final def declare(element: scala.xml.Elem): scala.xml.Elem = Xml.declareNamespace(this, element)

  // DOM
  final def ensureDeclared(element: org.w3c.dom.Element): Unit = if (!isDeclared(element)) declare(element)
  final def isDeclared(element: org.w3c.dom.Element): Boolean = Dom.isNamespaceDeclared(this, element)
  final def declare(element: org.w3c.dom.Element): Unit = Dom.declareNamespace(this, element)

  // SAX
  final def ensureDeclared(attributes: org.xml.sax.helpers.AttributesImpl): Unit = if (!isDeclared(attributes)) declare(attributes)
  final def isDeclared(attributes: org.xml.sax.helpers.AttributesImpl): Boolean = Sax.isNamespaceDeclared(this, attributes)
  final def declare(attributes: org.xml.sax.helpers.AttributesImpl): Unit = Sax.declareNamespace(this, attributes)
}

object Namespace {

  final class Prefixed(prefix: String, override val uri: String) extends Namespace {
    require((prefix != null) && prefix.nonEmpty)
    require((uri != null) && uri.nonEmpty)

    override def getPrefix: Option[String] = Some(prefix)
    override def getUri: Option[String] = Some(uri)
    override def qName(localName: String): String = {
      require(localName.nonEmpty)
      prefix + ":" + localName
    }
  }

  final class Default(override val uri: String) extends Namespace {
    require((uri != null) && uri.nonEmpty)

    override def getPrefix: Option[String] = None
    override def getUri: Option[String] = Some(uri)
    override def qName(localName: String): String = {
      require(localName.nonEmpty)
      localName
    }
  }

  object Xmlns extends Namespace {
    val prefix: String = "xmlns"
    override def getPrefix: Option[String] = Some(prefix)
    override def uri: String = "http://www.w3.org/2000/xmlns/"
    override def getUri: Option[String] = Some(uri)

    // Note: empty string attribute name is used for default namespace attributes.
    def qName(localName: String): String =
      prefix + (if (localName.isEmpty) "" else ":" + localName)
  }

  object No extends Namespace {
    override def toString: String = "<No Namespace>"
    override def getPrefix: Option[String] = None
    override def getUri: Option[String] = None
    override def uri: String = getUri.get

    def qName(localName: String): String = {
      require(localName.nonEmpty)
      localName
    }
  }

  def apply(
    prefix: Option[String],
    uri: Option[String]
  ): Namespace =
    if (prefix.isEmpty && uri.isEmpty) No else
    if (prefix.isEmpty && uri.isDefined) new Default(uri.get) else
      new Prefixed(prefix.get, uri.get)

  def apply(
    prefix: String,
    uri: String
  ): Namespace = Namespace(
    prefix = Option(prefix),
    uri = Option(uri)
  )

  // Scala XML
  def get(element: scala.xml.Elem): Namespace = Xml.getNamespace(element)
  def getAll(element: scala.xml.Elem): Seq[Namespace] = Xml.getNamespaces(element)

  // DOM
  def get(element: org.w3c.dom.Element): Namespace = Dom.getNamespace(element)
  def getAll(element: org.w3c.dom.Element): Seq[Namespace] = Dom.getNamespaces(element)

  // SAX
  def getAll(attributes: org.xml.sax.Attributes): Seq[Namespace] = Sax.getNamespaces(attributes)
}
