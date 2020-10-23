package org.opentorah.collector

abstract class ReportObject(site: Site) extends SimpleSiteObject(site) {
  override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)
}
