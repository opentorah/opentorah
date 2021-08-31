package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Stores, Selector, Store}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

object Reports extends By with Stores with HtmlContent[Collector] {
  override def selector: Selector = Selector.byName("report")

  override def findByName(name: String): Caching.Parser[Option[Store]] = findByName(
    fullName = name,
    findByName = name => findByNameInStores(name),
    allowedExtension = "html",
    assumeAllowedExtension = false
  )

  override val stores: Seq[Report[_]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(site: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(<div>{stores.map(report => <l>{report.a(site)(text = report.title)}</l>)}</div>)
}
