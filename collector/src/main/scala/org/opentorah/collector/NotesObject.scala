package org.opentorah.collector

import java.io.File
import org.opentorah.util.Files

// TODO add objects for every Markdown file?
final class NotesObject(site: Site) extends SiteObject(site) {
  def root: File = Files.url2file(site.store.urls.baseUrl).getParentFile // TODO ugh

  private def files: Seq[String] = Files.filesWithExtensions(new File(root, NotesObject.directoryName), "md")

  def resolve(parts: Seq[String]): Option[SiteFile] =
    if (parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      if (!extension.contains("html")) None
      else files.find(_ == fileName).map(mkNoteFile)
    }

  private def mkNoteFile(name: String): MarkdownSiteFile = new MarkdownSiteFile(
    name,
    mdFile = Files.file(root, Seq(NotesObject.directoryName, s"$name.md")),
    url = Seq(NotesObject.directoryName, s"$name.html")
  ) {
    override protected def style: String = "main"
    override protected def siteParameters: SiteParameters = site.siteParameters
  }

  def noteFiles: Seq[MarkdownSiteFile] = files.map(mkNoteFile)
}

object NotesObject {
  val directoryName: String = "notes"
}
