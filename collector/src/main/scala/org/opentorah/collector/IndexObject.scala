package org.opentorah.collector

import org.opentorah.tei.Tei

final class IndexObject(site: Site) extends SiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  override protected def teiUrl: Seq[String] = Seq("index.xml")

  override protected def teiWrapperUrl: Seq[String] = Seq("index.html")

  override protected def yaml: Seq[(String, String)] = Seq("windowName" -> CollectionObject.collectionViewer)

  override protected def tei: Tei = {
    val result =
      <head>Дела</head> ++
      <list type="bulleted">
        {for (collection <- site.collections.filterNot(collection =>
          Site.unpublished.contains(Site.collectionName(collection))))
        yield Site.toXml(collection)}
      </list>

    Tei(result)
  }
}
