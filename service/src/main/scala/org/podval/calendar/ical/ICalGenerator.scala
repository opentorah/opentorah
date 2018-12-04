package org.podval.calendar.ical

import java.io.{FileOutputStream, OutputStream}
import org.podval.calendar.dates.Calendar
import org.podval.calendar.gregorian.Gregorian
import org.podval.calendar.jewish.Jewish


final class ICalGenerator private(os: OutputStream) {
  import ICal._
  import ICalGenerator.{daysUrl, iconUrl}

  val out = new ICalWriter(os)

  private def writeYear(year: Int) {
    out.print(beginCalendar(
      "-//Podval Group//NONSGML Jewish Calendar//EN",
      Some("Jewish Dates"),
      Some("Jewish Dates, Events and Schedules")
    ))

    var dayG = Gregorian.Year(year).month(Gregorian.Month.Name.January).day(1)
    while (dayG.year.number == year) {
      out.print(day(dayG))
      dayG = dayG.next
    }

    out.print(endCalendar)
  }

  private def day(dayG: Gregorian.Day): Properties = {
    val dayJ = Calendar.toJewish(dayG)
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
  val baseUrl: String = "http://calendar.podval.org/"
  val daysUrl: String = baseUrl + "day/"
  val iconUrl: String = baseUrl + "favicon.ico"


  def main(args: Array[String]) {
    new ICalGenerator(new FileOutputStream("/tmp/jc.ics")).writeYear(2013)
  }
}
