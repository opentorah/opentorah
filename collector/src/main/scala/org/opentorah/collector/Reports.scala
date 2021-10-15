package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Store}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

object Reports extends By.Pure[Report[?]](selectorName = "report", storesPure = Report.reports), HtmlContent.ApparatusViewer[Collector]:
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Store.Path, collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(<div>{for report <- Report.reports yield <l>{HtmlContent.a(path :+ report)(text = report.title)}</l>}</div>)
