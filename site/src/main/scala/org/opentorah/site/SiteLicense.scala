package org.opentorah.site

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class SiteLicense(
  val name: String,
  val url: String,
)

object SiteLicense extends Element[SiteLicense]("license") {

  private val nameAttribute: Attribute.Required[String] = Attribute("name").required
  private val urlAttribute: Attribute.Required[String] = Attribute("url").required

  override def contentParsable: Parsable[SiteLicense] = new Parsable[SiteLicense] {
    override def parser: Parser[SiteLicense] = for {
      name <- nameAttribute()
      url <- urlAttribute()
    } yield new SiteLicense(
      name,
      url
    )

    override def unparser: Unparser[SiteLicense] = Unparser.concat[SiteLicense](
      nameAttribute(_.name),
      urlAttribute(_.url)
    )
  }
}

