package org.opentorah.collector

import java.io.File

abstract class MarkdownSiteFile(val name: String, mdFile: File, override val url: Seq[String]) extends SiteFile {
  override def viewer: Viewer = Viewer.Collection
  final override protected def navigationLinks: Seq[NavigationLink] = Seq.empty
  override protected def titleAndContent: TitleAndContent = ???
}
