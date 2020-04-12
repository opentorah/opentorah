package org.opentorah.collector

import org.opentorah.tei.Tei

final class IndexObject(site: Site) extends SimpleSiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def fileName: String = IndexObject.fileName

  override protected def yaml: Seq[(String, String)] = Seq("windowName" -> CollectionObject.collectionViewer)

  override protected def tei: Tei = {
    val result =
      <head>{IndexObject.title}</head> ++
      <list type="bulleted">
        {for (collection <- site.collections.filterNot(collection =>
          Site.unpublished.contains(Site.collectionName(collection))))
        yield Site.toXml(collection)}
      </list>

    Tei(result)
  }
}

object IndexObject {
  val fileName: String = "index"

  val title: String = "Дела"
}
