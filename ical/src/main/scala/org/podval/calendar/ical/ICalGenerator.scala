package org.podval.calendar.ical

import org.podval.calendar.dates.jewish.Jewish
import org.podval.calendar.dates.gregorian.Gregorian
import org.podval.calendar.dates.Conversions

import java.io.{OutputStream, FileOutputStream}


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
    val dayJ = Conversions.toJewish(dayG)
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
  val iconUrl: String = baseUrl + "icon.gif"


  def main(args: Array[String]) {
    val x = Jewish /// XXX why do I need to do this?!
    val y = Gregorian

    new ICalGenerator(new FileOutputStream("/tmp/jc.ics")).writeYear(2013)
  }
}
