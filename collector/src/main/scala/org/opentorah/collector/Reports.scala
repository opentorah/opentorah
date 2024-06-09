package org.opentorah.collector

import org.opentorah.store.{By, Context, Path, Pure}
import org.opentorah.xml.{A, Caching, Parser, Xml}
import zio.ZIO

object Reports extends
  By.WithSelector[Report[?]](selectorName = "report"),
  Pure.With[Report[?]](storesPure = Report.reports):
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)

  override def content(path: Path, context: Context): Caching.Parser[Xml.Element] = for
    lines <- ZIO.foreach(Report.reports)((report: Report[?]) =>
      for a: A <- context.a(path :+ report) yield
        <l>{a(text = report.title)}</l>
    )
  yield
    <div>{lines}</div>
