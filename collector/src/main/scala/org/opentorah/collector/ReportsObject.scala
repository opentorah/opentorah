package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.util.Files
import scala.xml.Node

final class ReportsObject(site: Site) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)

  override protected def fileName: String = ReportsObject.fileName

  override protected def viewer: Viewer = Viewer.Names

  override protected def teiBody: Seq[Node] =
    <head xmlns={Tei.namespace.uri}>{ReportsObject.title}</head> ++
    <l xmlns={Tei.namespace.uri}><ref target={Files.mkUrl(new MisnamedEntitiesReport(site).htmlUrl)}>{MisnamedEntitiesReport.title}</ref></l> ++
    <l xmlns={Tei.namespace.uri}><ref target={Files.mkUrl(new NoRefsReport(site).htmlUrl)}>{NoRefsReport.title}</ref></l>
}

// TODO make list of report(factory)s
object ReportsObject {
  val directoryName: String = "reports"

  val fileName: String = "index"

  val title: String = "Отчеты"

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || (parts == Seq(fileName))) Some(new ReportsObject(site).htmlFile) else
    if (parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      val result: Option[SimpleSiteObject] = fileName match {
        case MisnamedEntitiesReport.fileName => Some(new MisnamedEntitiesReport(site))
        case NoRefsReport.fileName => Some(new NoRefsReport(site))
        case _ => None
      }
      result.flatMap(k => SimpleSiteObject.resolve(extension, k))
    }
}
