package org.opentorah.collector

import org.opentorah.store.WithPath
import scala.xml.Node

final class TreeIndexObject(site: Site) extends SiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def teiUrl: Seq[String] = Seq(Site.collectionsFileName + ".xml")

  override protected def teiWrapperUrl: Seq[String] = Seq(Site.collectionsFileName + ".html")

  override protected def yaml: Seq[(String, String)] = Seq("title" -> "Архивы")

  // TODO write new, truly hierarchical index!
  override protected def xml: Seq[Node] = {
    val byArchive: Map[String, Seq[WithPath[Collection]]] =
      site.collections.groupBy(collection => Site.collectionArchive(collection).getOrElse(""))
    <head>Архивы</head> ++
    <list>{
      for (archive <- byArchive.keys.toList.sorted) yield {
        <item>
          <p>{s"[$archive]"}</p>
          <list type="bulleted">{for (collection <- byArchive(archive)) yield Site.toXml(collection)}</list>
        </item>}}
    </list>
  }
}
