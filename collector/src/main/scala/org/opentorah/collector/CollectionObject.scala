package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.util.Files
import scala.xml.Node

final class CollectionObject(site: Site, collection: WithPath[Collection]) extends SiteObject(site) {
  override def viewer: String = Site.collectionViewer

  override def teiFile: TeiFile = new TeiFile(this) {
    override def url: Seq[String] = ???
    override protected def xml: Seq[Node] = ???
  }

  override def teiWrapperFile: TeiWrapperFile = new TeiWrapperFile(this) {
    override def url: Seq[String] = ???
  }
}

object CollectionObject {

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = if (parts.isEmpty) None else {
    val collectionName: String = parts.head
    site.collections.find(collection => Site.fileName(collection.value) == collectionName).flatMap { collection =>
      if (parts.tail.isEmpty) Some(new CollectionObject(site, collection).teiWrapperFile)
      else parts.tail.head match {
        case Site.documentsDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "html").map(_.teiWrapperFile)
        case Site.teiDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "xml").map(_.teiFile)
        case Site.facsDirectoryName =>
          DocumentObject.resolve(site, collection, parts.tail.tail, "html").map(_.facsFile)

        // TODO actually resolve the collection!
        case file => if (parts.tail.tail.nonEmpty) None else {
          val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
          if (fileName != "index") None
          else SiteFile.resolve(extension, new CollectionObject(site, collection))
        }
      }
    }
  }
}
