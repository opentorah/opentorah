package org.opentorah.collector

import org.opentorah.xml.Xml

abstract class ReportObject[T](site: Site) extends SimpleSiteObject(site) {

  final override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)

  final override protected def teiBody: Seq[Xml.Node] = lines.map(lineToXml)

  protected def lines: Seq[T]

  protected def lineToXml(line: T): Xml.Element
}
