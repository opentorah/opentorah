package org.opentorah.collector

import scala.xml.Elem

final class SiteParameters(
  val title: String,
  val author: String,
  val email: String,
  val faviconJpg: String,
  val googleAnalyticsId: Option[String],
  val navigationLinks: Seq[NavigationLink],
  val footerCol3: Elem,
  val homeTarget: Viewer,
  val githubUsername: Option[String],
  val twitterUsername: Option[String]
)
