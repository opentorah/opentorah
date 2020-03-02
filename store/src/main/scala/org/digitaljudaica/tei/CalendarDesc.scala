package org.digitaljudaica.tei

import org.digitaljudaica.xml.{DescriptorRawXml, RawXml}
import scala.xml.Elem

final case class CalendarDesc(xml: Elem) extends RawXml(xml)

object CalendarDesc extends DescriptorRawXml[CalendarDesc]("calendarDesc", new CalendarDesc(_))
