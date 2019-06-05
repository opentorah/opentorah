package org.podval.docbook.gradle.xml

import org.w3c.dom.{Document, Element}
import org.xml.sax.helpers.AttributesImpl

class Namespace(val uri: String, val prefix: String = "") {
  override def toString: String = s"""$xmlns="$uri""""

  def withVersion(version: String): String = toString + s""" version="$version""""

  def isDefault: Boolean = prefix == ""

  def qName(localName: String): String = if (isDefault) localName else prefix + ":" + localName

  val default: Namespace = if (isDefault) this else new Namespace(uri)

  override def equals(other: Any): Boolean = other match {
    case that: Namespace =>
      val result: Boolean = this.is(that)
      if (result) require(this.prefix == that.prefix)
      result

    case _ => false
  }

  def is(document: Document): Boolean = is(document.getDocumentElement.getNamespaceURI)

  def is(namespace: Namespace): Boolean = is(namespace.uri)

  def is(namespaceUri: String): Boolean = namespaceUri == uri

  def ensureDeclared(element: Element): Unit =
    if (!isDeclared(element)) declare(element)

  private def isDeclared(element: Element): Boolean =
    element.getAttributeNS(Namespace.Xmlns.uri, prefix) == uri

  private def declare(element: Element): Unit =
    element.setAttributeNS(Namespace.Xmlns.uri, xmlns, uri)

  def ensureDeclared(attributes: AttributesImpl): Unit =
    if (!isDeclared(attributes)) declare(attributes)

  private def isDeclared(attributes: AttributesImpl): Boolean =
    attributes.getValue(Namespace.Xmlns.uri, prefix) == uri

  def declare(attributes: AttributesImpl): Unit =
    attributes.addAttribute(Namespace.Xmlns.uri, prefix, xmlns, "CDATA", uri)

  def xmlns: String =
    if (isDefault) Namespace.Xmlns.prefix else Namespace.Xmlns.qName(prefix)
}

object Namespace {
  object Xmlns extends Namespace(uri = "http://www.w3.org/2000/xmlns/", prefix = "xmlns")

  object Xml extends Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  object XInclude extends Namespace(uri = "http://www.w3.org/2001/XInclude", prefix = "xi")

  object XLink extends Namespace(uri = "http://www.w3.org/1999/xlink", prefix = "xlink")

  object Xsl extends Namespace(uri = "http://www.w3.org/1999/XSL/Transform", prefix = "xsl")
}
