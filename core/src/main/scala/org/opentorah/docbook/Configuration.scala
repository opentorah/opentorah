package org.opentorah.docbook

import org.opentorah.xml.Attribute

// TODO rename
private[docbook] object Configuration:
  val nameAttribute: Attribute.Required[String] = Attribute("name").required
  val valueAttribute: Attribute.Required[String] = Attribute("value").required
  val xslt1versionAttribute: Attribute.Optional[String] = Attribute("xslt1version").optional
  val xslt2versionAttribute: Attribute.Optional[String] = Attribute("xslt2version").optional
  val epubEmbeddedFontsAttribute: Attribute.Optional[String] = Attribute("epubEmbeddedFonts").optional
