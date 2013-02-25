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


object JewishCalendar extends Calendar {

  // XXX assignments of the companion objects have to happen early on, but even this is not sufficient!
  // I found that I need to assign JewishCalendar to a val to trigger its initialization - or I end up with a null for the Year companion object!
  override protected val timeCompanion = Time
  override protected val momentCompanion = Moment
  override protected val dayCompanion = Day
  override protected val monthCompanion = Month
  override protected val yearCompanion = Year


  type YearCharacter = (Boolean, Year.Kind)


  final class Year(number: Int) extends YearBase(number) {

    require(0 < number)


    def isLeap: Boolean = JewishCalendarConstants.isLeap(numberInCycle)


    // TODO give names to constants
    override def firstDay: Int = {
      val Adu = Set(1, 4, 6)
      def isAdu(day: Day) = Adu.contains(day.dayOfWeek)

      val newMoon = month(1).newMoon
      val day = newMoon.day
      val time = newMoon.time

      if (isAdu(day)) day.next // KH 7:1
      else if (time >= Time(18, 0)) {
        if (!isAdu(day.next)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
      }
      else if ((day.dayOfWeek == 3) && time >= Time( 9, 204) && !this.isLeap) day.next.next /* KH 7:4 */
      else if ((day.dayOfWeek == 2) && time >= Time(15, 589) && this.prev.isLeap) day.next /* KH 7:5 */
      else day
    }.number


    override def lengthInDays: Int = next.firstDay - this.firstDay


    override def firstMonth: Int = JewishCalendarConstants.MonthsInCycle*(cycle - 1) + firstMonthInCycle


    override def lengthInMonths: Int = JewishCalendarConstants.lengthInMonths(numberInCycle)


    def cycle: Int = ((number - 1) / JewishCalendarConstants.YearsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % JewishCalendarConstants.YearsInCycle) + 1


    def firstMonthInCycle: Int = JewishCalendarConstants.MonthsBeforeYearInCycle(numberInCycle - 1) + 1


    override def character: YearCharacter = (isLeap, kind)


    // KH 8:7,8
    def kind: Year.Kind = {
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
      val yearsBeforeCycle = (month.cycle - 1)*JewishCalendarConstants.YearsInCycle
      val yearMonthIsInCycle = JewishCalendarConstants.MonthsBeforeYearInCycle.count(_ < month.numberInCycle)
      Year(yearsBeforeCycle + yearMonthIsInCycle)
    }


    protected override def areYearsPositive: Boolean = true


    sealed trait Kind
    case object Short   extends Kind
    case object Regular extends Kind
    case object Full    extends Kind


    protected override def characters: Seq[YearCharacter] =
      for (isLeap <- Seq(true, false); kind <- Seq(Year.Short, Year.Regular, Year.Full)) yield (isLeap, kind)


    protected override def namesAndLengths(character: YearCharacter): List[(Month.Name, Int)] = character match { case (isLeap: Boolean, kind: Year.Kind) =>
      List(
        (Month.Tishrei, 30),
        (Month.Marheshvan, if (kind == Year.Full) 30 else 29),
        (Month.Kislev, if (kind == Year.Short) 29 else 30),
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


    def cycle: Int = ((number - 1) / JewishCalendarConstants.MonthsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % JewishCalendarConstants.MonthsInCycle) + 1


    def newMoon: Moment = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1)
  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    // XXX toString?!
    sealed trait Name
    case object Tishrei    extends Name
    case object Marheshvan extends Name
    case object Kislev     extends Name
    case object Teves      extends Name
    case object Shvat      extends Name
    case object Adar       extends Name
    case object Nisan      extends Name
    case object Iyar       extends Name
    case object Sivan      extends Name
    case object Tammuz     extends Name
    case object Av         extends Name
    case object Elul       extends Name
    case object AdarI      extends Name
    case object AdarII     extends Name


    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3)
    private val MeanLunarPeriod = Day(30).time(12, 793)


    // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
    // BeHaRaD: 5 hours 204 parts at night of the second day (KH 6:8)
    private val FirstNewMoon = Day(2).nightTime(5, 204)
  }


  final class Day(number: Int) extends DayBase(number) {

    def nightTime(hours: Int, parts: Int): Moment = time(Time.nightTime(hours, parts))


    def dayTime(hours: Int, parts: Int): Moment = time(Time.dayTime(hours, parts))
  }


  object Day extends DayCompanion {

    override def apply(number: Int): Day = new Day(number)


    override val FirstDayDayOfWeek = Constants.FirstDayDayOfWeekJewish
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
