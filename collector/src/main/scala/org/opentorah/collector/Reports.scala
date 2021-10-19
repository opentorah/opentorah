package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Path, Pure}
import org.opentorah.xml.{Parser, ScalaXml}
import zio.ZIO

object Reports extends
  By.WithSelector[Report[?]](selectorName = "report"),
  Pure.With[Report[?]](storesPure = Report.reports),
  HtmlContent.ApparatusViewer[Collector]:
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Path, collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(<div>{for report <- Report.reports yield <l>{collector.a(path :+ report)(text = report.title)}</l>}</div>)
