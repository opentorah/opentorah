package org.opentorah.html

import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace, ScalaXml}

object Html extends Dialect, Doctype:

  override val namespace: Namespace = Namespace(uri="http://www.w3.org/1999/xhtml", prefix="xhtml")

  override val mimeType: String = "text/html" // Note: and not "application/xhtml+xml"

  override val doctype: String = Doctype.string("html")

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")
  
  /*
  I tried to define CSS namespaces like this:
    @namespace tei   url("http://www.tei-c.org/ns/1.0");
    @namespace db    url("http://docbook.org/ns/docbook");
    @namespace xhtml url("http://www.w3.org/1999/xhtml");
  and use them in CSS rules like this: tei|div, docbook|title.

  It seems that in browser DOM all elements are in the HTML5 xhtml namespace
  unless xmlns attribute is present on that element;
  why are the namespace declarations not inherited is not clear.

  So, I prefix the names of the elements from non-HTML namespaces with the namespace prefix
  if their names clash with the HTML namespace in a way that makes CSS styling difficult.
  For instance, I use <div> to structure the layout, but need to be able to style TEI
  depending on the level of nesting of TEI divs.
  Also, HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
  it needs to not be an HTML <p> (and of course, namespace is ignored...)
  */
  val reservedElements: Set[String] = Set("head", "body", "title", "div", "p")

  val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

