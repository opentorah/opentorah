package org.opentorah.collector

import org.opentorah.html
import org.opentorah.site.Viewer

trait HtmlContent extends org.opentorah.site.HtmlContent[Site] {
  override def viewer: Viewer = Viewer.default

  final override def style: String = "/css/" + (if (isWide) "wide" else "main")

  def isWide: Boolean = false

  final def a(site: Site): html.a = site.a(this)

  def path(site: Site): Store.Path
}
