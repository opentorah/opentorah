package org.opentorah.collector

import scala.xml.Elem

class SiteParameters(
  val title: String,
  val author: String,
  val email: String,
  val faviconJpeg: String,
  val googleAnalyticsId: Option[String],
  val navigationLinks: Seq[NavigationLink],
  val footerCol3: Elem
)
