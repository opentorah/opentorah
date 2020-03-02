package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Node

final case class CalendarDesc(xml: Seq[Node]) extends RawXml(xml)

object CalendarDesc extends DescriptorRawXml[CalendarDesc]("calendarDesc", new CalendarDesc(_))
