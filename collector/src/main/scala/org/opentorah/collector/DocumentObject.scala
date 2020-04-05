package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.util.Files
import scala.xml.Node

// TODO path, collection etc.
final class DocumentObject(site: Site, document: Document) extends SiteObject(site) {
  override def viewer: String = Site.documentViewer

  override def teiFile: TeiFile = new TeiFile(this) {
    override def url: Seq[String] = ???
    override protected def xml: Seq[Node] = ???
  }

  override def teiWrapperFile: TeiWrapperFile = new TeiWrapperFile(this) {
    override def url: Seq[String] = ???
  }

  def facsFile: FacsFile = ???
}

object DocumentObject {

  def resolve(
    site: Site,
    collection: WithPath[Collection],
    parts: Seq[String],
    requiredExtension: String
  ): Option[DocumentObject] = if (parts.isEmpty || parts.tail.nonEmpty) None else {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
    if (!extension.contains(requiredExtension)) None else {
      val document: Option[Document] = collection.value.by.get.stores.find { document =>
        document.by.get.stores.exists(teiHolder => teiHolder.name == fileName)
      }
      document.map(document => new DocumentObject(site, document))
    }
  }
}
