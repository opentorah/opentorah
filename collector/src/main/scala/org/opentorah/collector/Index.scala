package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Path, Selector, Terminal}
import org.opentorah.xml.{Atom, Element, Nodes, Parser}
import zio.ZIO

sealed abstract class Index(name: String, selectorName: String) extends Terminal:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(Selector.getForName(selectorName).title.get)
  final override def htmlBodyTitle: Option[Nodes] = htmlHeadTitle.map(Atom.apply)

object Index:
  object Tree extends Index("collections", "archive"):
    override def content(path: Path, context: Context): Parser[Element] =
      val by: ByHierarchy = Collector.get(context).by
      by.treeIndex(Seq(by), context)

  object Flat extends Index("index", "case"):
    override def content(path: Path, context: Context): Parser[Element] = for
      collectionPaths: Seq[Path] <- Collector.get(context).collectionPaths
      lines <- ZIO.foreach(collectionPaths)((path: Path) =>
        val collection: Collection = Path.last[Collection](path)
        for a: A <- context.a(path) yield
          <li>
            {a(text = collection.pathHeaderHorizontal(path) + ": " + collection.titleString)}
            {collection.descriptionNodes}
          </li>
      )
    yield <ul>{lines}</ul>
