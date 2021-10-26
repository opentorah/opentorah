package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Path, Selector, Terminal}
import org.opentorah.xml.{Caching, Parser, ScalaXml}
import zio.ZIO

sealed abstract class Index(name: String, selectorName: String) extends Terminal, HtmlContent.DefaultViewer[Collector]:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(Selector.getForName(selectorName).title.get)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

object Index:
  object Tree extends Index("collections", "archive"):
    override def content(path: Path, collector: Collector): Parser[ScalaXml.Element] =
      ZIO.succeed(collector.by.treeIndex(Seq(collector.by), collector))

  object Flat extends Index("index", "case"):
    override def content(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] =
      for collectionPaths: Seq[Path] <- collector.collectionPaths yield
        <ul>{
          for path: Path <- collectionPaths yield
            val collection: Collection = path.last.asInstanceOf[Collection]
            <li>
              {collector.a(path)(text = collection.pathHeaderHorizontal(path) + ": " + collection.titleString)}
              {collection.descriptionNodes}
            </li>
        }</ul>
