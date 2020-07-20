package org.opentorah.xml

import org.w3c.dom.{Document, Element => DomElement}
import org.xml.sax.helpers.AttributesImpl

class Namespace(val uri: String, val prefix: String) {

  final override def equals(other: Any): Boolean = other match {
    case that: Namespace =>
      val result: Boolean = this.is(that)
      if (result) require(this.prefix == that.prefix)
      result

    case _ => false
  }

  final def is(document: Document): Boolean = is(document.getDocumentElement.getNamespaceURI)

  final def is(namespace: Namespace): Boolean = is(namespace.uri)

  final def is(namespaceUri: String): Boolean = namespaceUri == uri

  final override def toString: String = s"""$xmlns="$uri""""

  final def isDefault: Boolean = prefix == ""

  final def isTop: Boolean = isDefault && (uri == "")

  final def qName(localName: String): String = if (isDefault) localName else prefix + ":" + localName

  final lazy val default: Namespace = if (isDefault) this else new Namespace(uri, prefix = "")

  def ensureDeclared(element: DomElement): Unit =
    if (!isDeclared(element)) declare(element)

  private def isDeclared(element: DomElement): Boolean =
    element.getAttributeNS(Namespace.Xmlns.uri, prefix) == uri

  private def declare(element: DomElement): Unit =
    element.setAttributeNS(Namespace.Xmlns.uri, xmlns, uri)

  final def ensureDeclared(attributes: AttributesImpl): Unit =
    if (!isDeclared(attributes)) declare(attributes)

  private def isDeclared(attributes: AttributesImpl): Boolean =
    attributes.getValue(Namespace.Xmlns.uri, prefix) == uri

  final def declare(attributes: AttributesImpl): Unit = Attribute.addAttribute(
    uri = Namespace.Xmlns.uri,
    localName = prefix,
    qName = xmlns,
    value = uri,
    attributes
  )

  final def isXmlns: Boolean = prefix == Namespace.Xmlns.prefix

  private def xmlns: String =
    if (isDefault) Namespace.Xmlns.prefix else Namespace.Xmlns.qName(prefix)

  final def xmlnsAttribute: Attribute.Value[String] =
    Attribute(xmlns).withValue(uri)
}

object Namespace {
  object Top extends Namespace(uri = "", prefix = "")

  object Xmlns extends Namespace(uri = "http://www.w3.org/2000/xmlns/", prefix = "xmlns")

  object Xml extends Namespace(uri = "http://www.w3.org/XML/1998/namespace", prefix = "xml") {
    val header: String   = """<?xml version="1.0" encoding="UTF-8"?>"""
    val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""
  }

  object XInclude extends Namespace(uri = "http://www.w3.org/2001/XInclude", prefix = "xi")

  object XLink extends Namespace(uri = "http://www.w3.org/1999/xlink", prefix = "xlink")

  object Xsl extends Namespace(uri = "http://www.w3.org/1999/XSL/Transform", prefix = "xsl") {
    def version(usesDocBookXslt2: Boolean): String = if (usesDocBookXslt2) "2.0" else "1.0"
  }

  object Catalog extends Namespace(uri = "urn:oasis:names:tc:entity:xmlns:xml:catalog", prefix = "") {
    val dtdId: String = "-//OASIS//DTD XML Catalogs V1.1//EN"

    val dtdUri: String = "http://www.oasis-open.org/committees/entity/release/1.1/catalog.dtd"

    val doctype: String = s"""<!DOCTYPE catalog PUBLIC "$dtdId" "$dtdUri">"""
  }

  object MathML extends Namespace(uri = "http://www.w3.org/1998/Math/MathML", prefix = "mathml") {
    val mimeType: String = "application/mathml+xml"
  }

  object Svg extends Namespace(uri="http://www.w3.org/2000/svg", prefix="") {
    val mimeType: String = "image/svg+xml"
  }

  object DocBook extends Namespace(uri = "http://docbook.org/ns/docbook", prefix="") {
    val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

    val dtdUri: String = "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"

    val doctype: String = s"""<!DOCTYPE article PUBLIC "$dtdId" "$dtdUri">"""

    val version: String = "5.0"
  }

  object Tei extends Namespace(uri = "http://www.tei-c.org/ns/1.0", prefix="")
}
