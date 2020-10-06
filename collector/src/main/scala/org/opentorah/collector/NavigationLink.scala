package org.opentorah.collector

import org.opentorah.util.Files

final class NavigationLink private(
  val url: String,
  val title: String,
  val target: Option[String]
)

object NavigationLink {

  def apply(
    url: String,
    title: String,
    target: Option[String]
  ): NavigationLink = new NavigationLink(
    url + ".html",
    title,
    target
  )

  def apply(
    url: Seq[String],
    title: String,
    target: Option[String]
  ): NavigationLink = new NavigationLink(
    Files.mkUrl(url),
    title,
    target
  )
}
