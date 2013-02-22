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

package org.podval.calendar.dates


object JewishCalendar extends CalendarBase {

  // XXX assignments of the companion objects have to happen early on, but even this is not sufficient!
  // I found that I need to assign JewishCalendar to a val to trigger its initialization - or I end up with a null for the Year companion object!
  override protected val timeCompanion = Time
  override protected val momentCompanion = Moment
  override protected val dayCompanion = Day
  override protected val monthCompanion = Month
  override protected val yearCompanion = Year

  sealed trait MonthName


  sealed trait YearKind


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
        case 0 => Year.Short
        case 1 => Year.Regular
        case 2 => Year.Full
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


    case object Short   extends YearKind
    case object Regular extends YearKind
    case object Full    extends YearKind


    private val YearsInCycle = 19


    private val LeapYears = List(3, 6, 8, 11, 14, 17, 19)


    private def isLeap(numberInCycle: Int) = LeapYears.contains(numberInCycle)


    private def lengthInMonths(numberInCycle: Int): Int = if (isLeap(numberInCycle)) MonthsInLeapYear else MonthsInNonLeapYear


    private val MonthsInNonLeapYear = 12


    private val MonthsInLeapYear = MonthsInNonLeapYear + 1


    private val MonthsBeforeYearInCycle = ((1 to YearsInCycle) map (lengthInMonths(_))).scanLeft(0)(_ + _)


    val MonthsInCycle = MonthsBeforeYearInCycle.last


    private val Adu = Set(1, 4, 6)


    private def isAdu(day: Day) = Adu.contains(day.dayOfWeek)


    override def months(year: Year): List[MonthDescriptor] = {
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

    // XXX
//    private val Months: Map[(YearKind,Boolean), List[MonthDescriptor]] =
//      Map(
//      for (kind <- Seq(Short, Regular, Full); isLeap <- Seq(false, true)) yield (kind, isLeap) -> months(kind, isLeap)
//        : _*
//      )

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
        (Month.Tishrei, 30),
        (Month.Marheshvan, if (kind == Full) 30 else 29),
        (Month.Kislev, if (kind == Short) 29 else 30),
        (Month.Teves, 29),
        (Month.Shvat, 30)
      ) ++
      (if (!isLeap) List((Month.Adar, 29)) else List((Month.AdarI, 30), (Month.AdarII, 30))) ++
      List(
        (Month.Nisan, 30),
        (Month.Iyar, 29),
        (Month.Sivan, 30),
        (Month.Tammuz, 29),
        (Month.Av, 30),
        (Month.Elul, 29)
      )
    }
  }


  final class Month(number: Int) extends MonthBase(number) {

    override def numberInYear: Int = numberInCycle - year.firstMonthInCycle + 1


    def cycle: Int = ((number - 1) / Year.MonthsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % Year.MonthsInCycle) + 1


    def newMoon: Moment = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1)
  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


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


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    val MeanLunarPeriod = Day(30).time(12, 793)


    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
    // BeHaRaD: 5 hours 204 parts at night of the second day (KH 6:8)
    val FirstNewMoon = Day(2).nightTime(5, 204)
  }


  final class Day(number: Int) extends DayBase(number) {

    def nightTime(hours: Int, parts: Int): Moment = time(Time.nightTime(hours, parts))


    def dayTime(hours: Int, parts: Int): Moment = time(Time.dayTime(hours, parts))
  }


  object Day extends DayCompanion {

    override def apply(number: Int): Day = new Day(number)


    // It seems that first day of the first year was Sunday.
    override val FirstDayDayOfWeek = 1
  }


  final class Moment(days: Int, time: Time) extends MomentBase(days, time)


  object Moment extends MomentCompanion {

    override def apply(days: Int, time: Time): Moment = new Moment(days, time)
  }


  final class Time(hours: Int, parts: Int) extends TimeBase(hours, parts)


  object Time extends TimeCompanion {

    override def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def nightTime(hours: Int, parts: Int) = {
      require(hours < Constants.HoursPerHalfDay)
      Time(hours, parts)
    }


    def dayTime(hours: Int, parts: Int) = {
      require(hours < Constants.HoursPerHalfDay)
      Time(hours + Constants.HoursPerHalfDay, parts)
    }
  }
}
