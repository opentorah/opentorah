package org.opentorah.calendar.ical

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.Gregorian
import java.io.{FileOutputStream, OutputStream}

final class ICalGenerator private(os: OutputStream):
  val out: ICal = ICal(os)

  private def writeYear(year: Int): Unit =
    out.print(ICal.beginCalendar(
      "-//Podval Group//NONSGML Jewish Calendar//EN",
      Some("Jewish Dates"),
      Some("Jewish Dates, Events and Schedules")
    ))

    var dayG = Gregorian.Year(year).month(Gregorian.Month.January).day(1)
    while dayG.year.number == year do
      out.print(day(dayG))
      dayG = dayG.next

    out.print(ICal.endCalendar)

  private def day(dayG: Gregorian.Day): ICal.Properties =
    val dayJ = dayG.to(Jewish)
    val monthName: String = dayJ.month.name.name
    val dayNumber = dayJ.numberInMonth

    ICal.event(
      day = dayG,
      title = monthName + " " + dayNumber,
      icon = ICalGenerator.iconUrl,
      url = ICalGenerator.daysUrl + dayJ.year.number + "/" + monthName + "/" + dayNumber,
      width = 200,
      height = 200
    )


object ICalGenerator:
  val baseUrl: String = "http://calendar.opentorah.org/"
  val daysUrl: String = baseUrl + "day/"
  val iconUrl: String = baseUrl + "favicon.ico"

  def main(args: Array[String]): Unit =
    ICalGenerator(FileOutputStream("/tmp/jc.ics")).writeYear(2013)
