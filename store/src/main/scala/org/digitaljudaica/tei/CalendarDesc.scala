package org.digitaljudaica.tei

import org.digitaljudaica.xml.DescriptorRaw
import scala.xml.Elem

final case class CalendarDesc(xml: Elem)

object CalendarDesc extends DescriptorRaw[CalendarDesc](
  elementName = "calendarDesc",
  create = xml => new CalendarDesc(xml)
)
