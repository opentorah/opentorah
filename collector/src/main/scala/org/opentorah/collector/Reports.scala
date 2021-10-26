package org.opentorah.collector

import org.opentorah.store.{By, Context, Path, Pure, Viewer}
import org.opentorah.xml.{Caching, Parser, ScalaXml}
import zio.ZIO

object Reports extends
  By.WithSelector[Report[?]](selectorName = "report"),
  Pure.With[Report[?]](storesPure = Report.reports),
  Viewer.Apparatus:
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    pathShortener: Path.Shortener <- context.pathShortener
  yield
    <div>{
      for report <- Report.reports yield
        <l>{
          a(path :+ report, pathShortener)(text = report.title)
        }</l>
    }</div>
