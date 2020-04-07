package org.opentorah.collector

import org.opentorah.store.{Binding, By, Path, Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.xml.RawXml
import scala.xml.{Elem, Node}

final class HierarchyObject(site: Site, path: Path, store: Store) extends SiteObject(site) {
  override def viewer: String = CollectionObject.collectionViewer

  protected def teiUrl: Seq[String] = url("index.xml")

  protected def teiWrapperUrl: Seq[String] = url("index.html")

  private def url(file: String): Seq[String] =
    HierarchyObject.hierarchyDirectoryName +: HierarchyObject.segments(path) :+ file

  // TODO clean up:
  protected def tei: Tei = {
    val result =
      HierarchyObject.storeHeader(path, store) ++
      store.by.toSeq.flatMap { by: By[_] =>
        <p>
          <l>{Site.getName(by.selector.names)}:</l>
          <list type="bulleted">
            {by.stores.map { storeX =>
            val subStore = storeX.asInstanceOf[Store]  // TODO get rid of the cast!!!
            val title: Seq[Node] = RawXml.getXml(subStore.title)
            val titlePrefix: Seq[Node] = Site.textNode(Site.getName(subStore.names) + (if (title.isEmpty) "" else ": "))
            <item>
              {Site.ref(
              url =
                if (subStore.isInstanceOf[Collection]) Seq(CollectionObject.collectionsDirectoryName, Site.fileName(subStore))
                else HierarchyObject.path2url(path :+ by.selector.bind(subStore)),
              text = titlePrefix ++ title
            )}</item>
          }}
          </list>
        </p>
      }

    Tei(result)
  }
}

object HierarchyObject {

  val hierarchyDirectoryName: String = "by"

  // TODO I'd like to be able to start the Path with a top-level store in there (using "top" pseudo-selector?).
  def resolve(site: Site, path: Path, store: Store, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty) Some(new HierarchyObject(site, path, store).teiWrapperFile) else parts.head match {
      case "index.html" => Some(new HierarchyObject(site, path, store).teiWrapperFile)
      case "index.xml" => Some(new HierarchyObject(site, path, store).teiFile)
      case _ =>
        val selector = store.by.get.selector
        val selectorName: String = parts.head
        if (selector.names.find(selectorName).isEmpty) None else store match {
          case collection: Collection =>
            if (parts.tail.nonEmpty) None
            else Some(new CollectionObject(site, WithPath(path, collection)).teiWrapperFile)

          case _ => if (parts.tail.isEmpty) None else {
            val storeName: String = parts.tail.head.replace('_', ' ')
            store.by.get.stores.find(_.names.find(storeName).isDefined).flatMap { nextStore =>
              resolve(site, path :+ selector.bind(nextStore), nextStore, parts.tail.tail)
            }
          }
        }
    }

  // TODO move into general area
  private def storeHeader(path: Path, store: Store): Seq[Node] = {
    //    println(segments(store.path).mkString("/"))
    val isTop: Boolean = path.isEmpty
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] =
      if (isTop) Seq.empty else Site.textNode(
        Site.getName(path.last.selector.names) + " " + Site.getName(store.names) + (if (title.isEmpty) "" else ": ")
      )

    pathLinks(if (isTop) path else path.init) ++
      <head>{titlePrefix ++ title}</head> ++
      store.storeAbstract.map(value => <span>{value.xml}</span>).getOrElse(Seq.empty) ++
      RawXml.getXml(store.body)
  }

  // TODO move into general area
  private def pathLinks(path: Path): Seq[Elem] = for (ancestor <- path.path.inits.toSeq.reverse.tail) yield {
    val binding: Binding = ancestor.last
    val link: Elem = Site.ref(
      url = path2url(Path(ancestor)),
      text = Site.getName(binding.store.names)
    )
    val title: Seq[Node] = RawXml.getXml(binding.store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(Site.textNode(": "))
    <l>{Site.getName(binding.selector.names)} {link ++ titlePrefix ++ title}</l>
  }

  // TODO move into general area
  def path2url(path: Path): Seq[String] = hierarchyDirectoryName +: segments(path)

  // TODO move into general area
  def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(Site.getName)
      .map(_.replace(' ', '_'))
}
