package org.opentorah.collector

import org.opentorah.store.{Binding, Path, Store}
import org.opentorah.tei.Ref
import org.opentorah.util.{Files, Xml}
import org.opentorah.xml.RawXml
import scala.xml.{Elem, Node}

object Hierarchy {

  val directoryName: String = "by"

  def urlPrefix(path: Path): Seq[String] = directoryName +: segments(path)

  def segments(path: Path): Seq[String] =
    path.path.flatMap(binding => Seq(binding.selector.names, binding.store.names))
      .map(Site.getName)
      .map(Files.spacesToUnderscores)

  def storeHeader(path: Path, store: Store): Seq[Node] = {
    pathLinks(path) ++
    <head>{storeTitle(path, store)}</head> ++
    store.storeAbstract.map(value => <span>{value.xml}</span>).getOrElse(Seq.empty) ++
    RawXml.getXml(store.body)
  }

  def pathLinks(pathRaw: Path): Seq[Elem] = {
    val path: Path = if (pathRaw.isEmpty) pathRaw else pathRaw.init
    for (ancestor <- path.path.inits.toSeq.reverse.tail) yield {
      val binding: Binding = ancestor.last
      val link: Elem = Ref.toXml(
        target = urlPrefix(Path(ancestor)),
        text = Site.getName(binding.store.names)
      )
      <l>{Site.getName(binding.selector.names)} {link ++ storeTitle(binding.store)}</l>
    }
  }

  def storeTitle(path: Path, store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (path.isEmpty) Seq.empty else Xml.textNode(
      Site.getName(path.last.selector.names) + " " + Site.getName(store.names) + (if (title.isEmpty) "" else ": ")
    )

    titlePrefix ++ title
  }

  def storeTitle(store: Store): Seq[Node] = {
    val title: Seq[Node] = RawXml.getXml(store.title)
    val titlePrefix: Seq[Node] = if (title.isEmpty) Seq.empty else Seq(Xml.textNode(": "))
    titlePrefix ++ title
  }
}
