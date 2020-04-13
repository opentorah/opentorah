package org.opentorah.collector

import org.opentorah.tei.Tei

final class IndexObject(site: Site) extends SimpleSiteObject(site) {

  override protected def fileName: String = IndexObject.fileName

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  override protected def yaml: Seq[(String, String)] = Seq("windowName" -> teiWrapperViewer.name)

  override protected def tei: Tei = {
    val result =
      <head>{IndexObject.title}</head> ++
      <list type="bulleted">
        {for (collection <- site.collections.filterNot(collection =>
          Site.unpublishedCollections.contains(CollectionObject.collectionName(collection))))
        yield CollectionObject.collectionXml(collection)}
      </list>

    Tei(result)
  }
}

object IndexObject {
  val fileName: String = "index"

  val title: String = "Дела"
}
