package org.opentorah.collector

import scala.xml.{Elem, Node}

abstract class ReportObject[T](site: Site) extends SimpleSiteObject(site) {

  final override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)

  final override protected def teiBody: Seq[Node] = lines.map(lineToXml)

  protected def lines: Seq[T]

  protected def lineToXml(line: T): Elem
}
