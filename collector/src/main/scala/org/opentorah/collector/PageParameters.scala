package org.opentorah.collector

class PageParameters(
  val lang: String = "en",
  val style: String,
  val title: Option[String] = None,
  val target: Option[Viewer] = None,
  val navigationLinks: Seq[NavigationLink]
)
