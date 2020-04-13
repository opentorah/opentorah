package org.opentorah.collector

import org.opentorah.store.{Binding, By, Path, Selector, Store, WithPath}
import org.opentorah.tei.Ref
import org.opentorah.util.{Files, Xml}
import org.opentorah.xml.RawXml
import scala.xml.{Elem, Node}

final class HierarchyObject(site: Site, path: Path, store: Store) extends SimpleSiteObject(site) {

  override protected def fileName: String = HierarchyObject.fileName

  override protected def urlPrefix: Seq[String] = HierarchyObject.urlPrefix(path)

  override protected def teiWrapperViewer: Viewer = Viewer.Collection

  // TODO clean up:
  protected def teiBody: Seq[Node] = {
    HierarchyObject.storeHeader(path, store) ++
    store.by.toSeq.flatMap { by: By[_] =>
      <p>
        <l>{Site.getName(by.selector.names)}:</l>
        <list type="bulleted">
          {by.stores.map { storeX =>
          val subStore = storeX.asInstanceOf[Store]  // TODO get rid of the cast!!!
          val title: Seq[Node] = RawXml.getXml(subStore.title)
          val titlePrefix: Seq[Node] = Xml.textNode(Site.getName(subStore.names) + (if (title.isEmpty) "" else ": "))
          // TODO the path in the call to urlPrefix is not really correct...
          <item>
            {Ref.toXml(
            target = subStore match {
              case collection: Collection => CollectionObject.urlPrefix(WithPath(path, collection))
              case _ => HierarchyObject.urlPrefix(path :+ by.selector.bind(subStore))
            },
            text = titlePrefix ++ title
          )}</item>
        }}
        </list>
      </p>
    }
  }
}

object HierarchyObject {

  val directoryName: String = "by"

  private def urlPrefix(path: Path): Seq[String] = directoryName +: segments(path)

  val fileName: String = "index"

  // TODO I'd like to be able to start the Path with a top-level store in there (using "top" pseudo-selector?).
  def resolve(site: Site, path: Path, store: Store, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty) Some(new HierarchyObject(site, path, store).teiWrapperFile) else
      SimpleSiteObject.resolve(Some(parts.head), new HierarchyObject(site, path, store)).orElse {
        val selector: Selector = store.by.get.selector
        val selectorName: String = parts.head
        if (selector.names.find(selectorName).isEmpty) None else store match {
          case collection: Collection =>
            if (parts.tail.nonEmpty) None
            else Some(new CollectionObject(site, WithPath(path, collection)).teiWrapperFile)

          case _ => if (parts.tail.isEmpty) None else {
            val storeName: String = Files.underscoresToSpaces(parts.tail.head)
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
      if (isTop) Seq.empty else Xml.textNode(
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
    val link: Elem = Ref.toXml(
      target = urlPrefix(Path(ancestor)),
      text = Site.getName(binding.store.names)
    )
    val title: Seq[Node] = RawXml.getXml(binding.store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(Xml.textNode(": "))
    <l>{Site.getName(binding.selector.names)} {link ++ titlePrefix ++ title}</l>
  }

  // TODO move into general area
  private def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(Site.getName)
      .map(Files.spacesToUnderscores)
}
