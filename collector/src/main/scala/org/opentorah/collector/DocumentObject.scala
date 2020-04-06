package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.util.Files
import scala.xml.Node

final class DocumentObject(site: Site, collection: WithPath[Collection], document: Document) extends SiteObject(site) {
  override def viewer: String = Site.documentViewer

  def facsFile: SiteFile = ???

  override protected def teiUrl: Seq[String] = ???

  override protected def xml: Seq[Node] = ???

  override protected def teiWrapperUrl: Seq[String] = ???
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
      // TODO move search into Collection (and other searches like this into appropriate classes)
      val document: Option[Document] = collection.value.by.get.stores.find { document =>
        document.by.get.stores.exists(teiHolder => teiHolder.name == fileName)
      }
      document.map(document => new DocumentObject(site, collection, document))
    }
  }
}
