package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.Node

final class IndexObject(site: Site) extends SimpleSiteObject(site) {

  override protected def fileName: String = IndexObject.fileName

  override protected def viewer: Viewer = Viewer.Collection

  override protected def teiBody: Seq[Node] = {
    <head xmlns={Tei.namespace.uri}>{IndexObject.title}</head> ++
    <list xmlns={Tei.namespace.uri} type="bulleted">
      {for (collection <- site.publishedCollections) yield Hierarchy.collectionXml(site, collection)}
    </list>
  }
}

object IndexObject {
  val fileName: String = "index"

  private val title: String = "Дела"
}
