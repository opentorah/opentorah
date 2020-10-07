package org.opentorah.collector

import org.opentorah.util.Files

// TODO take SiteObject as the parameter
final class NavigationLink private(
  val url: String,
  val title: String,
  val target: Option[Viewer]
)

object NavigationLink {

  def apply(
    url: String,
    title: String,
    target: Option[Viewer]
  ): NavigationLink = new NavigationLink(
    url + ".html",
    title,
    target
  )

  def apply(
    url: Seq[String],
    title: String,
    target: Option[Viewer]
  ): NavigationLink = new NavigationLink(
    Files.mkUrl(url),
    title,
    target
  )
}
