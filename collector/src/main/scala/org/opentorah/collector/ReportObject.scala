package org.opentorah.collector

import org.opentorah.util.Files

abstract class ReportObject(site: Site) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = Seq(ReportObject.directoryName)
}

object ReportObject {

  val directoryName: String = "reports"

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      val result: Option[SimpleSiteObject] = fileName match {
        case MisnamedEntitiesReport.fileName => Some(new MisnamedEntitiesReport(site))
        case NoRefsReport          .fileName => Some(new NoRefsReport          (site))
        case _ => None
      }
      result.flatMap(k => SimpleSiteObject.resolve(extension, k))
    }
}
