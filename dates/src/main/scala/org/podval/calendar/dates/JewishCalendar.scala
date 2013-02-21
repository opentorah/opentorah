package org.podval.calendar.dates

/*
 * Copyright 2011-2013 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait JewishCalendar extends CalendarBase {

  sealed trait MonthName
  case object Tishrei    extends MonthName
  case object Marheshvan extends MonthName
  case object Kislev     extends MonthName
  case object Teves      extends MonthName
  case object Shvat      extends MonthName
  case object Adar       extends MonthName
  case object Nisan      extends MonthName
  case object Iyar       extends MonthName
  case object Sivan      extends MonthName
  case object Tammuz     extends MonthName
  case object Av         extends MonthName
  case object Elul       extends MonthName
  case object AdarI      extends MonthName
  case object AdarII     extends MonthName


  // XXX Rename "ShortYear" etc.
  // XXX Why did I fail to put this into the Year object?
  sealed trait YearKind
  case object Short   extends YearKind
  case object Regular extends YearKind
  case object Full    extends YearKind


  final class Year(number: Int) extends YearBase(number) {

    require(0 < number)


    override def isLeap: Boolean = Year.isLeap(numberInCycle)


    // TODO give names to constants
    override def firstDay: Int = {
      val newMoon = month(1).newMoon
      val day = newMoon.day
      val time = newMoon.time

      if (Year.isAdu(day)) day.next // KH 7:1
      else if (time >= Time(18, 0)) {
        if (!Year.isAdu(day.next)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
      }
      else if ((day.dayOfWeek == 3) && time >= Time( 9, 204) && !this.isLeap) day.next.next /* KH 7:4 */
      else if ((day.dayOfWeek == 2) && time >= Time(15, 589) && this.prev.isLeap) day.next /* KH 7:5 */
      else day
    }.number


    override def lengthInDays: Int = next.firstDay - this.firstDay


    override def firstMonth: Int = Year.MonthsInCycle*(cycle - 1) + firstMonthInCycle


    override def lengthInMonths: Int = Year.lengthInMonths(numberInCycle)


    def cycle: Int = ((number - 1) / Year.YearsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % Year.YearsInCycle) + 1


    def firstMonthInCycle: Int = Year.MonthsBeforeYearInCycle(numberInCycle - 1) + 1


    // KH 8:7,8
    def kind: YearKind = {
      val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

      daysOverShort match {
        case 0 => Short
        case 1 => Regular
        case 2 => Full
        case _ => throw new IllegalArgumentException("Impossible year length " + lengthInDays + " for " + this)
      }
    }
  }


  object Year extends YearCompanion {

    override def apply(number: Int): Year = new Year(number)


    override def apply(month: Month): Year = {
      val yearsBeforeCycle = (month.cycle - 1)*YearsInCycle
      val yearMonthIsInCycle = MonthsBeforeYearInCycle.count(_ < month.numberInCycle)
      Year(yearsBeforeCycle + yearMonthIsInCycle)
    }


    override def apply(day: Day): Year = {
      // TODO give names to constants
      val yearForSureNotAfter = (4 * day.number / (4 * 365 + 1)) - 1
      var result = Year(scala.math.max(1, yearForSureNotAfter))
      require(result.firstDay <= day.number)
      while (result.next.firstDay <= day.number) result = result.next
      result
    }


    private val YearsInCycle = 19


    private val LeapYears = List(3, 6, 8, 11, 14, 17, 19)


    private def isLeap(numberInCycle: Int) = LeapYears.contains(numberInCycle)


    private def lengthInMonths(numberInCycle: Int): Int = if (isLeap(numberInCycle)) MonthsInLeapYear else MonthsInNonLeapYear


    private val MonthsInNonLeapYear = 12


    private val MonthsInLeapYear = MonthsInNonLeapYear + 1


    private val MonthsBeforeYearInCycle = ((1 to YearsInCycle) map (lengthInMonths(_))).scanLeft(0)(_ + _)


    val MonthsInCycle = MonthsBeforeYearInCycle.last


    private val Adu = List(1, 4, 6)


    private def isAdu(day: Day) = Adu.contains(day.dayOfWeek)


    def months(year: Year): List[MonthDescriptor] = {
      val kind = year.kind
      val isLeap = year.isLeap

      (kind, isLeap) match {
        case (Short, false) => shortMonths
        case (Short, true) => shortLeapMonths
        case (Regular, false) => regularMonths
        case (Regular, true) => regularLeapMonths
        case (Full, false) => fullMonths
        case (Full, true) => fullLeapMonths
      }
    }


    // XXX redo using a Map[(kind, isLeap), Seq[MonthDescriptor]]?
    private val shortMonths: List[MonthDescriptor] = months(Short, false)
    private val shortLeapMonths: List[MonthDescriptor] = months(Short, true)
    private val regularMonths: List[MonthDescriptor] = months(Regular, false)
    private val regularLeapMonths: List[MonthDescriptor] = months(Regular, true)
    private val fullMonths: List[MonthDescriptor] = months(Full, false)
    private val fullLeapMonths: List[MonthDescriptor] = months(Full, true)


    private def months(kind: YearKind, isLeap: Boolean): List[MonthDescriptor] = {
      val namesAndLengths = this.namesAndLengths(kind, isLeap)
      val (names, lengths) = namesAndLengths.unzip
      val daysBefore = lengths.scanLeft(0)(_ + _).init
      (namesAndLengths zip daysBefore) map (m => new MonthDescriptor(m._1._1, m._1._2, m._2))
    }


    private def namesAndLengths(kind: YearKind, isLeap: Boolean) = {
      List(
        (Tishrei, 30),
        (Marheshvan, if (kind == Full) 30 else 29),
        (Kislev, if (kind == Short) 29 else 30),
        (Teves, 29),
        (Shvat, 30)
      ) ++
        (if (!isLeap)
          List((Adar, 29)) else
          List((AdarI, 30), (AdarII, 30))
          ) ++
        List(
          (Nisan, 30),
          (Iyar, 29),
          (Sivan, 30),
          (Tammuz, 29),
          (Av, 30),
          (Elul, 29)
        )
    }
  }


  override protected val yearCompanion = Year


  final class Month(number: Int) extends MonthBase(number) {

    require(0 < number)


    override def year: Year = Year(this)


    override def numberInYear: Int = numberInCycle - year.firstMonthInCycle + 1


    def cycle: Int = ((number - 1) / Year.MonthsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % Year.MonthsInCycle) + 1


    def newMoon: Moment = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1)

  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    val MeanLunarPeriod = Day(30).time(12, 793)


    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
    // BeHaRaD: 5 hours 204 parts at night of the second day (KH 6:8)
    val FirstNewMoon = Day(2).nightTime(5, 204)
  }


  override protected val monthCompanion = Month


  final class Day(number: Int) extends DayBase(number) {

    override def dayOfWeek: Int = Day.dayOfWeek(number)


    override def year: Year = Year(this)


    def nightTime(hours: Int, parts: Int): Moment = time(Time.nightTime(hours, parts))


    def dayTime(hours: Int, parts: Int): Moment = time(Time.dayTime(hours, parts))
  }


  object Day extends DayCompanion {

    override def apply(number: Int): Day = new Day(number)


    def dayOfWeek(day: Int): Int = ((day + FirstDayDayOfWeek - 1 - 1) % DaysPerWeek) + 1
  }


  override protected val dayCompanion = Day


  final class Moment(days: Int, time: Time) extends MomentBase(days, time)


  object Moment extends MomentCompanion {

    override def apply(days: Int, time: Time): Moment = new Moment(days, time)
  }


  override protected val momentCompanion = Moment


  final class Time(hours: Int, parts: Int) extends TimeBase(hours, parts)


  object Time extends TimeCompanion {

    override def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def nightTime(hours: Int, parts: Int) = {
      require(hours < TimeT.HoursPerHalfDay)
      Time(hours, parts)
    }


    def dayTime(hours: Int, parts: Int) = {
      require(hours < TimeT.HoursPerHalfDay)
      Time(hours + TimeT.HoursPerHalfDay, parts)
    }
  }


  override protected val timeCompanion = Time
}
