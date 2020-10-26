package org.opentorah.collector

final class PageParameters(
  val lang: String = "en",
  val style: String,
  val headTitle: Option[String],
  val title: Option[String],
  val target: Option[Viewer],
  val navigationLinks: Seq[NavigationLink]
)
