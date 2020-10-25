package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.{Elem, Node}

abstract class ReportObject[T](site: Site) extends SimpleSiteObject(site) {

  final override protected def urlPrefix: Seq[String] = Seq(ReportsObject.directoryName)

  final override protected def teiBody: Seq[Node] =
    <head xmlns={Tei.namespace.uri}>{title.get}</head> ++ lines.map(lineToXml)

  protected def lines: Seq[T]

  protected def lineToXml(line: T): Elem
}
