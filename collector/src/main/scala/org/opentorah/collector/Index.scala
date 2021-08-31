package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Selector, Store}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

sealed abstract class Index(name: String, selectorName: String) extends Store with HtmlContent[Collector] {
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(Selector.byName(selectorName).title.get)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)
}

object Index {
  object Tree extends Index("collections", "archive") {
    override def content(site: Collector): Parser[ScalaXml.Element] =
      ZIO.succeed(site.by.treeIndex(site))
  }

  object Flat extends Index("index", "case") {
    override def content(site: Collector): Parser[ScalaXml.Element] =
      ZIO.succeed(<ul>{site.collections.map(collection => <li>{collection.flatIndexEntry(site)}</li>)}</ul>)
  }
}
