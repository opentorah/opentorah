package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import Jewish.{Day, Month, Year}
import Month.Name._

final class DatesTest extends FlatSpec {

  "known dates" should "have correct day of the week" in {
    assertResult(Day.Name.Sheni)(Day(5772, Marheshvan, 24).name)
  }

  "conversions from date to days and back" should "end where they started" in {
    test(1   , Tishrei,  1)
    test(2   , Tishrei,  1)
    test(5768, AdarII , 28)
    test(5769, Nisan  , 14)
  }

  private def test(yearNumber: Int, monthName: Month.Name, dayNumber: Int) {
    val year = Year(yearNumber)
    assertResult(yearNumber)(year.number)

    val month = year.month(monthName)
    assertResult(monthName)(month.name)

    val day = month.day(dayNumber)

    assertResult(year)(day.year)
    assertResult(month)(day.month)
    assertResult(dayNumber)(day.numberInMonth)
  }
}
