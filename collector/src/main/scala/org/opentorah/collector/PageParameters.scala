package org.opentorah.collector

class PageParameters(
  val lang: String = "en",
  val style: String, // "main",
  val title: Option[String] = None,
  val target: Option[String] = None,
  val navigationLinks: Seq[NavigationLink]
)
