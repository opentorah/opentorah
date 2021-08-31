package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

object Reports extends By with Stores.Terminal with HtmlContent[Collector] {
  override def selector: Selector = Selector.byName("report")

  private val reports: Seq[Report[_]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)

  override protected def terminalStores: Seq[Store.Terminal] = reports

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(<div>{reports.map(report => <l>{report.a(collector)(text = report.title)}</l>)}</div>)
}
