package org.opentorah.calendar.ical

import java.io.{OutputStream, PrintStream}

final class ICalWriter(os: OutputStream) {
  val out = new PrintStream(os)

  def print(properties: ICal.Properties): Unit = ICal.print(properties, out)
}
