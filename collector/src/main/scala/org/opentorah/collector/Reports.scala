package org.opentorah.collector

import org.opentorah.site.{By, Caching, HtmlContent, Selector, Store}
import org.opentorah.xml.{Parser, Xml}
import zio.ZIO

object Reports extends By with HtmlContent[Site] {
  override def selector: Selector = Selector.byName("report")

  override def findByName(name: String): Caching.Parser[Option[Store]] = Store.findByName(
    name,
    "html",
    name => Store.findByName(name, reports)
  )

  val reports: Seq[Report[_]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)
  override def acceptsIndexHtml: Boolean = true

  override def content(site: Site): Parser[Xml.Element] =
    ZIO.succeed(<div>{reports.map(report => <l>{report.a(site)(text = report.title)}</l>)}</div>)
}
