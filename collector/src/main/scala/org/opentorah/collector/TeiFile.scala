package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.{Elem, Node}

// TODO add ".xml" to the last segment
abstract class TeiFile(siteObject: SiteObject) extends SiteFile(siteObject) {

  final def content: String = {
    val elem: Elem = Site.processTei(Tei.toXml(TeiUtil.addCommonNoCalendar(Tei(xml))), siteObject.site)

    """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
    TeiUtil.teiPrettyPrinter.render(elem) +
    "\n"
  }

  protected def xml: Seq[Node]
}
