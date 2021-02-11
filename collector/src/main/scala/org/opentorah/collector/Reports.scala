package org.opentorah.collector

import org.opentorah.xml.Xml

object Reports extends By with HtmlContent {
  override def selector: Selector = Selector.byName("report")

  override def findByName(name: String): Option[Store] = Store.findByName(
    name,
    "html",
    name => Store.findByName(name, reports)
  )

  val reports: Seq[Report[_]] = Seq(Report.NoRefs, Report.MisnamedEntities)

  override def viewer: Viewer = Viewer.Names
  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)
  override def acceptsIndexHtml: Boolean = true

  override def path(site: Site): Store.Path = Seq(Reports)

  override def content(site: Site): Xml.Element =
    <div>{for (report <- reports) yield <l>{report.a(site)(text = report.title)}</l>}</div>
}
