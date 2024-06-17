package org.opentorah.site

import org.opentorah.xml.{Attribute, ElementTo, Parsable}

object SitePage extends ElementTo[String]("page"):
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required

  override def contentParsable: Parsable[String] = urlAttribute
