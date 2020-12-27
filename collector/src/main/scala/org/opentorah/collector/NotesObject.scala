package org.opentorah.collector

import java.net.URL
import org.opentorah.store.FilesList
import org.opentorah.util.Files

final class NotesObject(site: Site) extends SiteObject(site) {

  private val (fileNames: Seq[String], fileInDirectory: (String => URL)) = FilesList.get(
    baseUrl = site.store.urls.baseUrl,
    directoryName = NotesObject.directoryName,
    listName = None,
    extension = "md"
  )

  def resolve(parts: Seq[String]): Option[SiteFile] =
    if (parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      if (!extension.contains("html")) None
      else fileNames.find(_ == fileName).map(mkNoteFile)
    }

  private def mkNoteFile(name: String): MarkdownSiteFile = new MarkdownSiteFile(
    name,
    mdFile = Files.url2file(fileInDirectory(name)), // TODO generalize to URL!
    url = Seq(NotesObject.directoryName, s"$name.html")
  ) {
    override protected def style: String = "main"
    override protected def siteParameters: SiteParameters = site.siteParameters
  }
}

object NotesObject {
  val directoryName: String = "notes"
}
