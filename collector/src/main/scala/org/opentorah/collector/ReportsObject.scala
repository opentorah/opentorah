package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.Xml

final class ReportsObject(site: Site) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)

  override def fileName: String = "index"

  override protected def viewer: Viewer = Viewer.Names

  override def title: Option[String] = Some("Отчеты")

  override protected def teiBody: Seq[Xml.Node] = simpleSubObjects.map(line)

  private def line(report: SimpleSiteObject): Xml.Element =
    <l xmlns={Tei.namespace.uri}><ref target={Files.mkUrl(report.url)}>{report.title.get}</ref></l>

  def resolve(parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || (parts == Seq(fileName))) Some(htmlFile) else
      if (parts.tail.nonEmpty) None else {
        val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
        simpleSubObjects
          .find(_.fileName == fileName)
          .flatMap(k => SimpleSiteObject.resolve(extension, k))
      }

  // TODO generalize
  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq(
    new MisnamedEntitiesReport(site),
    new NoRefsReport(site)
  )
}

object ReportsObject {
  val directoryName: String = "reports"
}
