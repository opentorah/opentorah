package org.opentorah.collector

import org.opentorah.site.Viewer

trait HtmlContent extends org.opentorah.site.HtmlContent[Site] {
  override def viewer: Viewer = Viewer.default
}
