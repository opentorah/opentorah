package org.opentorah.calendar.ical

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.Gregorian
import java.io.{FileOutputStream, OutputStream}

final class ICalGenerator private(os: OutputStream) {
  import ICal._
  import ICalGenerator.{daysUrl, iconUrl}

  val out = new ICalWriter(os)

  private def writeYear(year: Int): Unit = {
    out.print(beginCalendar(
      "-//Podval Group//NONSGML Jewish Calendar//EN",
      Some("Jewish Dates"),
      Some("Jewish Dates, Events and Schedules")
    ))

    var dayG = Gregorian.Year(year).month(Gregorian.Month.January).day(1)
    while (dayG.year.number == year) {
      out.print(day(dayG))
      dayG = dayG.next
    }

    out.print(endCalendar)
  }

  private def day(dayG: Gregorian.Day): Properties = {
    val dayJ = dayG.to(Jewish)
    val monthName: String = dayJ.month.name.name
    val dayNumber = dayJ.numberInMonth
    val summaryText = monthName + " " + dayNumber
    val url = daysUrl + dayJ.year.number + "/" + monthName + "/" + dayNumber

    val result: Properties =
      summary(summaryText) ++
      fullDayDuration(dayG) ++
      googleContent(summaryText, iconUrl, url, 200, 200)

    event(transparent = true, result)
  }
}


object ICalGenerator {
  val baseUrl: String = "http://calendar.opentorah.org/"
  val daysUrl: String = baseUrl + "day/"
  val iconUrl: String = baseUrl + "favicon.ico"


  def main(args: Array[String]): Unit = {
    new ICalGenerator(new FileOutputStream("/tmp/jc.ics")).writeYear(2013)
  }
}
