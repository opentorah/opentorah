package org.opentorah.collectorng

final class EntityFile(val id: String) extends SiteFile {
  override def name: String = s"$id.html"
  override def content: String = ???
}
