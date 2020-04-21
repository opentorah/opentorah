package org.opentorah.collector

import org.opentorah.entity.EntityReference
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{Binding, Path, Store, WithPath}
import org.opentorah.tei.Ref
import org.opentorah.util.{Files, Xml}
import org.opentorah.xml.RawXml
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
    <head>{storeTitle(path, store)}</head> ++
    store.storeAbstract.map(value => Seq(<span>{value.xml}</span>)).getOrElse(Seq.empty) ++
    RawXml.getXml(store.body)

  private def pathLinks(pathRaw: Path): Seq[Elem] = {
    val path: Path = if (pathRaw.isEmpty) pathRaw else pathRaw.init
    for (ancestor <- path.path.inits.toSeq.reverse.tail) yield {
      val binding: Binding = ancestor.last
      val link: Elem = Ref.toXml(
        target = urlPrefix(Path(ancestor)),
        text = getName(binding.store.names)
      )
      <l>{getName(binding.selector.names)} {link ++ storeTitle(binding.store)}</l>
    }
  }

  private def storeTitle(path: Path, store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (path.isEmpty) Seq.empty else Xml.textNode(
      getName(path.last.selector.names) + " " + getName(store.names) + (if (title.isEmpty) "" else ": ")
    )

    titlePrefix ++ title
  }

  def storeTitle(store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(Xml.textNode(": "))
    titlePrefix ++ title
  }

  // TODO eliminate
  def storeName(store: Store): String =
    store.names.name

  // TODO eliminate
  def referenceCollectionName(reference: WithPath[EntityReference]): String =
    storeName(reference.path.init.init.last.store)

  // TODO eliminate
  def collectionXml(collection: WithPath[Collection]): Elem =
    <item>{Ref.toXml(
      target = CollectionObject.teiWrapperUrl(collection),
      text = fullName(collection.path) + Xml.toString(storeTitle(collection.value))
      )}<lb/>
      <abstract>{collection.value.storeAbstract.get.xml}</abstract>
    </item>

  def getName(names: Names): String = names.doFind(Language.Russian.toSpec).name

  def fileName(store: Store): String =
    Files.nameAndExtension(Files.pathAndName(store.urls.fromUrl.get.getPath)._2)._1
}
