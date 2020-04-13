package org.opentorah.collector

import org.opentorah.store.WithPath
import scala.xml.Node

final class TreeIndexObject(site: Site) extends SimpleSiteObject(site) {

  override protected def fileName: String = TreeIndexObject.fileName

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  override protected def yaml: Seq[(String, String)] = Seq(
    "windowName" -> teiWrapperViewer.name,
    "title" -> TreeIndexObject.title
  )

  override protected def teiBody: Seq[Node] = {
    val byArchive: Map[String, Seq[WithPath[Collection]]] =
      site.collections.groupBy(collection => CollectionObject.collectionArchive(collection).getOrElse(""))

    <head>{TreeIndexObject.title}</head> ++
    <list>{
      for (archive <- byArchive.keys.toList.sorted) yield {
        <item>
          <p>{s"[$archive]"}</p>
          <list type="bulleted">{for (collection <- byArchive(archive)) yield CollectionObject.collectionXml(collection)}</list>
        </item>}}
    </list>
  }
}

object TreeIndexObject {
  val fileName: String = "collections"

  val title: String = "Архивы"
}
