package org.podval.docbook.gradle

import org.w3c.dom.{Document, Element}
import org.xml.sax.helpers.AttributesImpl

sealed class Namespace(val uri: String, val prefix: String = "") {
  override def toString: String = s"""$xmlns="$uri""""

  def isDefault: Boolean = prefix == ""

  def qName(localName: String): String = if (isDefault) localName else prefix + ":" + localName

  val default: Namespace = if (isDefault) this else new Namespace(uri)

  def is(document: Document): Boolean = is(document.getDocumentElement.getNamespaceURI)

  def is(namespace: Namespace): Boolean = is(namespace.uri)

  def is(namespaceUri: String): Boolean = namespaceUri == uri

  def ensureDeclared(element: Element): Unit =
    if (!isDeclared(element)) declare(element)

  private def isDeclared(element: Element): Boolean =
    element.getAttributeNS(Namespace.Xmlns.uri, prefix) == uri

  private def declare(element: Element): Unit =
    element.setAttributeNS(Namespace.Xmlns.uri, xmlns, uri)

  def declare(attributes: AttributesImpl): Unit =
    attributes.addAttribute(Namespace.Xmlns.uri, prefix, xmlns, null, uri)

  def xmlns: String =
    if (isDefault) Namespace.Xmlns.prefix else Namespace.Xmlns.qName(prefix)
}

object Namespace {
  object Xmlns extends Namespace(uri = "http://www.w3.org/2000/xmlns/", prefix = "xmlns")

  object Xml extends Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml")

  object XInclude extends Namespace(uri = "http://www.w3.org/2001/XInclude", prefix = "xi")

  object XLink extends Namespace(uri = "http://www.w3.org/1999/xlink", prefix = "xlink")

  object DocBook extends Namespace(uri = "http://docbook.org/ns/docbook")

  // Note: only MathJaxObj.getNormalNamespacePrefix() needs the prefix;
  // everywhere else default mapping is assumed.
  object MathML extends Namespace(uri = "http://www.w3.org/1998/Math/MathML", prefix = "mathml") {

    val mimeType: String = "application/mathml+xml"

    val math: String = "math"
    val mrow: String = "mrow"
    val mi: String = "mi"
  }

  object MathJax extends Namespace(uri = "http://podval.org/mathjax/ns/ext", prefix = "mathjax")

  object SVG extends Namespace(uri = "http://www.w3.org/2000/svg") {
    val mimeType: String = "image/svg+xml"
  }
}
