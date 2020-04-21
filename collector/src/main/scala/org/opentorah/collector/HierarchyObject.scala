package org.opentorah.collector

import org.opentorah.store.{By, Path, Selector, Store, WithPath}
import org.opentorah.tei.Ref
import org.opentorah.util.{Files, Xml}
import scala.xml.Node

final class HierarchyObject(site: Site, path: Path, store: Store) extends SimpleSiteObject(site) {

  override protected def fileName: String = HierarchyObject.fileName

  override protected def urlPrefix: Seq[String] = Hierarchy.urlPrefix(path)

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  protected def teiBody: Seq[Node] = Hierarchy.storeHeader(path, store) ++ store.by.toSeq.flatMap { by: By[_] =>
    <p>
      <l>{Hierarchy.getName(by.selector.names)}:</l>
      <list type="bulleted">{by.stores.map(_.asInstanceOf[Store]).map { store => // TODO get rid of the cast!!!
        val storePath: Path = path :+ by.selector.bind(store)
        <item>{Ref.toXml(
          target = store match {
            case collection: Collection => CollectionObject.urlPrefix(WithPath(storePath, collection))
            case _ => Hierarchy.urlPrefix(storePath)
          },
          text = Xml.textNode(Hierarchy.getName(store.names)) ++ Hierarchy.storeTitle(store)
        )}</item>
      }}</list>
    </p>
  }
}

object HierarchyObject {

  val fileName: String = "index"

  // TODO I'd like to be able to start the Path with a top-level store in there (using "top" pseudo-selector?).
  def resolve(site: Site, path: Path, store: Store, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty) Some(new HierarchyObject(site, path, store).teiWrapperFile) else {
      val head: String = parts.head
      SimpleSiteObject.resolve(Some(head), new HierarchyObject(site, path, store)) orElse {
        if (store.by.isEmpty) None else {
          val by: By[Store] = store.by.get
          val selector: Selector = by.selector
          if (selector.names.find(head).isEmpty) None else store match {
            case collection: Collection =>
              if (parts.tail.nonEmpty) None
              else Some(new CollectionObject(site, WithPath(path, collection)).teiWrapperFile)

            case _ => if (parts.tail.isEmpty) None else {
              val storeName: String = Files.underscoresToSpaces(parts.tail.head)
              by.stores.find(_.names.find(storeName).isDefined).flatMap { nextStore =>
                resolve(site, path :+ selector.bind(nextStore), nextStore, parts.tail.tail)
              }
            }
          }
        }
      } orElse {
        val (fileName: String, extension: Option[String]) = Files.nameAndExtension(head)
        if (fileName != HierarchyObject.fileName) None
        else SimpleSiteObject.resolve(extension, new HierarchyObject(site, path, store))
      }
    }
}
