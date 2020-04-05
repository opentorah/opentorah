package org.opentorah.collector

import org.opentorah.store.{Binding, By, Path, Store, WithPath}
import org.opentorah.xml.RawXml
import scala.xml.{Elem, Node}

final class HierarchyObject(site: Site, path: Path, store: Store) extends SiteObject(site) {
  override def viewer: String = Site.collectionViewer

  override def teiFile: TeiFile = new TeiFile(this) {
    override def url: Seq[String] = HierarchyObject.hierarchyDirectoryName +: HierarchyObject.segments(path) :+ "index.xml"

    override protected def xml: Seq[Node] = HierarchyObject.store2xml(WithPath(path, store))
  }

  override def teiWrapperFile: TeiWrapperFile = new TeiWrapperFile(this) {
    override def url: Seq[String] =  HierarchyObject.hierarchyDirectoryName +: HierarchyObject.segments(path) :+ "index.html"
  }
}

object HierarchyObject {

  val hierarchyDirectoryName: String = "by"

  // TODO I need to be able to start the Path with a top-level store in there (using "top" pseudo-selector?).
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

  // TODO clean up; do Seq() for URLs...
  def store2xml(store: WithPath[Store]): Seq[Node] = {
    storeHeader(store) ++
      store.value.by.toSeq.flatMap { by: By[_] =>
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
                if (subStore.isInstanceOf[Collection]) s"/${Site.collectionsDirectoryName}/${Site.fileName(subStore)}"
                else path2url(store.path :+ by.selector.bind(subStore)),
              text = titlePrefix ++ title
            )}</item>
          }}
          </list>
        </p>
      }
  }

  private def storeHeader(store: WithPath[Store]): Seq[Node] = {
    //    println(segments(store.path).mkString("/"))
    val isTop: Boolean = store.path.isEmpty
    val title: Seq[Node] = RawXml.getXml(store.value.title)
    val titlePrefix: Seq[Node] =
      if (isTop) Seq.empty else Site.textNode(
        Site.getName(store.path.last.selector.names) + " " + Site.getName(store.value.names) + (if (title.isEmpty) "" else ": ")
      )

    pathLinks(if (isTop) store.path else store.path.init) ++
      <head>{titlePrefix ++ title}</head> ++
      store.value.storeAbstract.map(value => <span>{value.xml}</span>).getOrElse(Seq.empty) ++
      RawXml.getXml(store.value.body)
  }

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

  def path2url(path: Path): String =
    "/" + hierarchyDirectoryName + "/" + segments(path).mkString("", "/", "/")

  def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(Site.getName)
      .map(_.replace(' ', '_'))
}
