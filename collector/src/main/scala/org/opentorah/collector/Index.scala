package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Path, Selector, Terminal, Viewer}
import org.opentorah.xml.{Caching, Parser, ScalaXml}
import zio.ZIO

sealed abstract class Index(name: String, selectorName: String) extends Terminal, Viewer.Default:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(Selector.getForName(selectorName).title.get)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

object Index:
  object Tree extends Index("collections", "archive"):
    override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
      pathShortener: Path.Shortener <- context.pathShortener
    yield
      val by: ByHierarchy = Collector.get(context).by
      by.treeIndex(Seq(by), context, pathShortener)

  object Flat extends Index("index", "case"):
    override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
      collectionPaths: Seq[Path] <- Collector.get(context).collectionPaths
      pathShortener: Path.Shortener <- context.pathShortener
    yield
      <ul>{
        for path: Path <- collectionPaths yield
          val collection: Collection = Path.last[Collection](path)
          <li>
            {a(path, pathShortener)(text = collection.pathHeaderHorizontal(path) + ": " + collection.titleString)}
            {collection.descriptionNodes}
          </li>
      }</ul>
