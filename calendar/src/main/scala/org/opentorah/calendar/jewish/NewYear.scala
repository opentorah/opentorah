package org.opentorah.calendar.jewish

import Jewish.{Day, Moment, TimeVector}
import org.opentorah.calendar.Week

object NewYear:

  sealed class Delay(val days: Int) derives CanEqual // all deriveds are objects; using eq

  object Delay:
    case object No extends Delay(0)

    // KH 7:1
    case object Adu extends Delay(1):
      val onDays: Set[Week.Day] = Set(Week.Day.Rishon, Week.Day.Rvii, Week.Day.Shishi)

      def applies(day: Week.Day): Boolean = onDays.contains(day)

    // KH 7:1-2 (molad zoken)
    case object First extends Delay(1):
      val time: TimeVector = TimeVector().hours(18)

      def applies(time: TimeVector): Boolean =
        time >= this.time

    // KH 7:3
    case object FirstAndAdu extends Delay(2)

    // KH 7:4
    case object Second extends Delay(2):
      val day: Week.Day = Week.Day.Shlishi
      val time: TimeVector = TimeVector().hours(9).parts(204)

      def applies(yearNumber: Int, day: Week.Day, time: TimeVector): Boolean =
        (day == this.day) &&
        (time >= this.time) &&
        !LeapYearsCycle.isLeapYear(yearNumber)

    // KH 7:5
    case object Third extends Delay(1):
      val day: Week.Day = Week.Day.Sheni
      val time: TimeVector = TimeVector().hours(15).parts(589)

      def applies(yearNumber: Int, day: Week.Day, time: TimeVector): Boolean =
        (day == this.day) &&
        (time >= this.time) &&
        // This is not defined for year 0 - and doesn't apply :)
        LeapYearsCycle.isLeapYear(yearNumber-1)

  val delaysEnabledFromYear: Int = 5

  // Note: to have Friday of the creation of Adam and Eve be the first of Tishrei,
  // I had to suppress New Year delays for a few first years.
  final def delay(yearNumber: Int, newMoon: Moment): Delay = if yearNumber < delaysEnabledFromYear then Delay.No else
    val dayNumber: Int = newMoon.dayNumber
    val time: TimeVector = newMoon.time
    val day: Week.Day = name(dayNumber)

    if Delay.Adu.applies(day) then Delay.Adu
    else if Delay.First.applies(time) then
      if Delay.Adu.applies(name(dayNumber+1)) then Delay.FirstAndAdu
      else Delay.First
    else if Delay.Second.applies(yearNumber, day, time) then Delay.Second
    else if Delay.Third .applies(yearNumber, day, time) then Delay.Third
    else Delay.No

  final def name(dayNumber: Int): Week.Day = Week.Day.forNumber(Day.numberInWeek(dayNumber))
