package org.opentorah.site

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class SiteLicense(
  val name: String,
  val url: String,
)

object SiteLicense extends Element[SiteLicense]("license"):
  override def contentParsable: Parsable[SiteLicense] = new Parsable[SiteLicense]:
    private val nameAttribute: Attribute.Required[String] = Attribute("name").required
    private val urlAttribute: Attribute.Required[String] = Attribute("url").required
  
    override def parser: Parser[SiteLicense] = for
      name: String <- nameAttribute()
      url: String <- urlAttribute()
    yield SiteLicense(
      name,
      url
    )

    override def unparser: Unparser[SiteLicense] = Unparser.concat[SiteLicense](
      nameAttribute(_.name),
      urlAttribute(_.url)
    )

