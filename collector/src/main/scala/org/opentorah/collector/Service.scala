package org.opentorah.collector

object Service extends org.opentorah.site.Service[Site] {
  override def siteReader: org.opentorah.site.SiteReader[Site] = Site
  override def projectId: String = "alter-rebbe-2"
  override def bucketName: String = "store.alter-rebbe.org"
}
