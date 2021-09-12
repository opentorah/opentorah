package org.opentorah.site

import org.opentorah.xml.{Attribute, Element, Parsable}

object SitePage extends Element[String]("page"):
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required

  override def contentParsable: Parsable[String] = urlAttribute
