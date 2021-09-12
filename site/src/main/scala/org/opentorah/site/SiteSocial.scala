package org.opentorah.site

import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

final class SiteSocial(
  val github : Option[String],
  val twitter: Option[String]
):
  def list: Seq[(SiteSocial.Service, String)] = Seq(
    optional(SiteSocial.GitHub , github ),
    optional(SiteSocial.Twitter, twitter)
  ).flatten

  private def optional(service:SiteSocial.Service, username: Option[String]): Option[(SiteSocial.Service, String)] =
    username.map(username => service -> username)

object SiteSocial extends Element[SiteSocial]("social"):

  sealed class Service(val serviceUrl: String, val iconUrl: String)
  object GitHub  extends Service(serviceUrl = "https://github.com"     , iconUrl = "/assets/icons.svg#github" )
  object Twitter extends Service(serviceUrl = "https://www.twitter.com", iconUrl = "/assets/icons.svg#twitter")

  val empty: SiteSocial = SiteSocial(
    github = None,
    twitter = None
  )

  private val githubAttribute : Attribute.Optional[String] = Attribute("github" ).optional
  private val twitterAttribute: Attribute.Optional[String] = Attribute("twitter").optional

  override def contentParsable: Parsable[SiteSocial] = new Parsable[SiteSocial]:
    override def parser: Parser[SiteSocial] = for
      github : Option[String]  <- githubAttribute ()
      twitter: Option[String] <- twitterAttribute()
    yield SiteSocial(
      github,
      twitter
    )

    override def unparser: Unparser[SiteSocial] = Unparser.concat[SiteSocial](
      githubAttribute (_.github ),
      twitterAttribute(_.twitter)
    )
