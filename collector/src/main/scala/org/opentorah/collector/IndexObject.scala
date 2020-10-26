package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.Node

final class IndexObject(site: Site) extends SimpleSiteObject(site) {

  override def fileName: String = "index"

  override protected def viewer: Viewer = Viewer.Collection

  override def title: Option[String] = Some("Дела")

  override protected def teiBody: Seq[Node] = {
    <list xmlns={Tei.namespace.uri} type="bulleted">
      {for (collection <- site.publishedCollections) yield Hierarchy.collectionXml(site, collection)}
    </list>
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}
