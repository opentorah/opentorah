package org.opentorah.collector

import org.opentorah.store.{By, Path, Store, WithPath}
import org.opentorah.tei.{Ref, Tei}
import org.opentorah.xml.Xml
import scala.xml.Node

final class TreeIndexObject(site: Site) extends SimpleSiteObject(site) {

  override def fileName: String = "collections"

  override protected def viewer: Viewer = Viewer.Collection

  override def title: Option[String] = Some("Архивы")

  // TODO there is now no link from here to the root of the hierarchy:
  //   Ref.toXml(new HierarchyObject(site, Path.empty, site.store).htmlFile.url, TreeIndexObject.title)
  // add it somewhere - or wait until 'by' is merged with the 'collections'...
  // Aslo, maybe add hierarchy links to the 'by' pages...
  override protected def teiBody: Seq[Node] = listForStore(Path.empty, site.store)

  private def listForStore(path: Path, store: Store): Seq[Node] = store.by.toSeq.flatMap { by: By[_] =>
    <list xmlns={Tei.namespace.uri} type="none">
      <item><emph>{Hierarchy.getName(by.selector.names)}</emph></item>
      <item><list type="none">
          {by.stores.map(_.asInstanceOf[Store]).map { store => // TODO get rid of the cast!!!
            val isCollection: Boolean = store.isInstanceOf[Collection]
            val storePath: Path = path :+ by.selector.bind(store)
            val siteObject: SiteObjectWithFile =
              if (isCollection) new CollectionObject(site, WithPath(path, store.asInstanceOf[Collection]))
              else new HierarchyObject(site, storePath, store)
            <item>
              {Ref.toXml(siteObject.htmlFile.url, Hierarchy.getName(store.names) + Xml.toString(Hierarchy.storeTitle(store)))}
              {if (isCollection) Seq.empty else listForStore(storePath, store)}
            </item>
        }}
      </list></item>
    </list>
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}
