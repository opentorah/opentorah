package org.opentorah.collector

import org.opentorah.xml.Xml

final class SiteParameters(
  val title: String,
  val author: String,
  val email: String,
  val faviconJpg: String,
  val googleAnalyticsId: Option[String],
  val navigationLinks: Seq[NavigationLink],
  val footerCol3: Xml.Element,
  val homeTarget: Viewer,
  val githubUsername: Option[String],
  val twitterUsername: Option[String]
)
