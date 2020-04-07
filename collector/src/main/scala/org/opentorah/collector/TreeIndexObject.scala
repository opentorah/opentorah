package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.Tei

final class TreeIndexObject(site: Site) extends SiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def teiUrl: Seq[String] = Seq(TreeIndexObject.collectionsFileName + ".xml")

  override protected def teiWrapperUrl: Seq[String] = Seq(TreeIndexObject.collectionsFileName + ".html")

  override protected def yaml: Seq[(String, String)] = Seq("title" -> "Архивы")

  // TODO write new, truly hierarchical index!
  override protected def tei: Tei = {
    val byArchive: Map[String, Seq[WithPath[Collection]]] =
      site.collections.groupBy(collection => Site.collectionArchive(collection).getOrElse(""))

    val result =
      <head>Архивы</head> ++
      <list>{
        for (archive <- byArchive.keys.toList.sorted) yield {
          <item>
            <p>{s"[$archive]"}</p>
            <list type="bulleted">{for (collection <- byArchive(archive)) yield Site.toXml(collection)}</list>
          </item>}}
      </list>

    Tei(result)
  }
}

object TreeIndexObject {
  val collectionsFileName: String = "collections"
}
