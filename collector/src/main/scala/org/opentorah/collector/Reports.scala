package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.By
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

object Reports extends By.Pure[Report[?]](selectorName = "report", storesPure = Report.reports), HtmlContent[Collector]:
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(<div>{Report.reports.map(report => <l>{report.a(collector)(text = report.title)}</l>)}</div>)
