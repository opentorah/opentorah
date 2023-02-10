package org.opentorah.xml

import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace, PrettyPrinter}

object Html extends Dialect, Doctype:

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1999/xhtml", prefix = "xhtml")

  override val mimeType: String = "text/html" // Note: and not "application/xhtml+xml"

  override val doctype: String = Doctype.string("html")

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements =
      Set("nav", "header", "main", "div"),
    clingyElements =
      Set("a"),
    // Some elements are mis-processed when they are empty, e.g. <script .../> ...
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data"),
    preformattedElements = Set("pre")
  )
