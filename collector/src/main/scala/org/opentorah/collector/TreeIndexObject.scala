package org.opentorah.collector

import org.opentorah.store.WithPath
import org.opentorah.tei.Tei

final class TreeIndexObject(site: Site) extends SimpleSiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def fileName: String = TreeIndexObject.collectionsFileName

  override protected def yaml: Seq[(String, String)] = Seq("title" -> TreeIndexObject.title)

  override protected def tei: Tei = {
    val byArchive: Map[String, Seq[WithPath[Collection]]] =
      site.collections.groupBy(collection => Site.collectionArchive(collection).getOrElse(""))

    val result =
      <head>{TreeIndexObject.title}</head> ++
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

  val title: String = "Архивы"
}
