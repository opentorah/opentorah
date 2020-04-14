package org.opentorah.collector

import org.opentorah.store.{By, Store}
import scala.xml.Node

final class TreeIndexNgObject(site: Site) extends SimpleSiteObject(site) {

  override protected def fileName: String = TreeIndexNgObject.fileName

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  override protected def yaml: Seq[(String, String)] = Seq(
    "windowName" -> teiWrapperViewer.name,
    "title" -> TreeIndexNgObject.title
  )

  override protected def teiBody: Seq[Node] = {
    <head>{TreeIndexNgObject.title}</head> ++
    listForStore(site.store)
  }

  private def listForStore(store: Store): Seq[Node] = store.by.toSeq.flatMap { by: By[_] =>
    <l>{Site.getName(by.selector.names)}:</l> ++
    <list type="bulleted">
      {by.stores.map(_.asInstanceOf[Store]).map { store => // TODO get rid of the cast!!!
      <item></item>
    }}
    </list>
  }
}

object TreeIndexNgObject {
  val fileName: String = "collections-ng"

  val title: String = "Архивы"
}
