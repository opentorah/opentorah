package org.opentorah.html

import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace}

object Html extends Dialect, Doctype:

  override val namespace: Namespace = Namespace(uri="http://www.w3.org/1999/xhtml", prefix="xhtml")

  override val mimeType: String = "text/html" // Note: and not "application/xhtml+xml"

  override val doctype: String = "<!DOCTYPE html>"

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")

  val reservedElements: Set[String] = Set("head", "body")

  val reservedAttributes: Set[String] = Set("class", "target", "lang")
