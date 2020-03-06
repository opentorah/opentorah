package org.opentorah.tei

import org.opentorah.xml.RawXml
import scala.xml.Node

final case class CalendarDesc(xml: Seq[Node]) extends RawXml(xml)

object CalendarDesc extends RawXml.Descriptor[CalendarDesc]("calendarDesc", new CalendarDesc(_))
