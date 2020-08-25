package org.opentorah.collector

import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Binding, Path, Store, WithPath}
import org.opentorah.tei.{EntityReference, Ref, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.{RawXml, Xml}
import scala.xml.{Elem, Node}

object Hierarchy {

  val directoryName: String = "by"

  def urlPrefix(path: Path): Seq[String] = directoryName +: segments(path)

  private def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(getName)
      .map(Files.spacesToUnderscores)

  def fullName(path: Path): String = path.path.map { binding =>
    getName(binding.selector.names) + " " + getName(binding.store.names)
  }.mkString(", ")

  def storeHeader(path: Path, store: Store): Seq[Node] =
    pathLinks(path) ++
    <head xmlns={Tei.namespace.uri}>{storeTitle(path, store)}</head> ++
    store.storeAbstract.map(value => Seq(<span xmlns={Tei.namespace.uri}>{value.xml}</span>)).getOrElse(Seq.empty) ++
    RawXml.getXml(store.body)

  private def pathLinks(pathRaw: Path): Seq[Elem] = {
    val path: Path = if (pathRaw.isEmpty) pathRaw else pathRaw.init
    for (ancestor <- path.path.inits.toSeq.reverse.tail) yield {
      val binding: Binding = ancestor.last
      val link: Elem = Ref.toXml(
        target = urlPrefix(Path(ancestor)),
        text = getName(binding.store.names)
      )
      <l xmlns={Tei.namespace.uri}>{getName(binding.selector.names)} {link ++ storeTitle(binding.store)}</l>
    }
  }

  private def storeTitle(path: Path, store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (path.isEmpty) Seq.empty else Xml.mkText(
      getName(path.last.selector.names) + " " + getName(store.names) + (if (title.isEmpty) "" else ": ")
    )

    titlePrefix ++ title
  }

  def storeTitle(store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(Xml.mkText(": "))
    titlePrefix ++ title
  }

  // TODO eliminate
  def storeName(store: Store): String =
    store.names.name

  // TODO eliminate
  def referenceCollectionName(reference: WithPath[EntityReference]): String =
    storeName(reference.path.init.init.last.store)

  // TODO eliminate
  def collectionXml(site: Site, collection: WithPath[Collection]): Elem =
  // TODO make a Ref serializer that takes SiteObject...
    <item xmlns={Tei.namespace.uri}>{Ref.toXml(
      target = new CollectionObject(site, collection).teiWrapperFile.url,
      text = fullName(collection.path) + Xml.toString(storeTitle(collection.value))
      )}<lb/>
      <abstract>{collection.value.storeAbstract.get.xml}</abstract>
    </item>

  def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

  def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1

  val pathOrdering: Ordering[Path] = (x: Path, y: Path) => Ordering.Iterable[Binding]((x: Binding, y: Binding) => {
    val selectorCompare: Int = x.selector.names.name.toLowerCase compare y.selector.names.name.toLowerCase
    if (selectorCompare != 0) selectorCompare
    else x.store.names.name.toLowerCase compare y.store.names.name.toLowerCase
  }).compare(x.path, y.path)
}
