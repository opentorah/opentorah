package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Selector, Store}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

sealed abstract class Index(name: String, selectorName: String) extends Store.Terminal, HtmlContent.DefaultViewer[Collector]:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(Selector.getForName(selectorName).title.get)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

object Index:
  object Tree extends Index("collections", "archive"):
    override def content(path: Store.Path, collector: Collector): Parser[ScalaXml.Element] =
      ZIO.succeed(collector.by.treeIndex( Seq.empty/* TODO Seq(Tree)? */, collector))

  object Flat extends Index("index", "case"):
    override def content(path: Store.Path, collector: Collector): Parser[ScalaXml.Element] =
      ZIO.succeed(<ul>{for collection <- collector.collections yield <li>{collection.flatIndexEntry(collector)}</li>}</ul>)
