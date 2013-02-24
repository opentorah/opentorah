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


object GregorianCalendar extends Calendar {

  // XXX assignments of the companion objects have to happen early on, but even this is not sufficient!
  // I found that I need to assign JewishCalendar to a val to trigger its initialization - or I end up with a null for the Year companion object!
  override protected val timeCompanion = Time
  override protected val momentCompanion = Moment
  override protected val dayCompanion = Day
  override protected val monthCompanion = Month
  override protected val yearCompanion = Year


  sealed trait MonthName


  type YearCharacter = Boolean


  final class Year(number: Int) extends YearBase(number) {

//    require(0 < number)

    // TODO give names to constants?
    def isLeap: Boolean = (number % 4 == 0) && ((number % 100 != 0) || (number % 400 == 0))


    override def firstDay: Int =
      GregorianCalendarConstants.DaysInNonLeapYear * (number - 1) + (number - 1)/4 - (number - 1)/100 + (number - 1)/400 + 1


    override def lengthInDays = if (isLeap) GregorianCalendarConstants.DaysInLeapYear else GregorianCalendarConstants.DaysInNonLeapYear


    override def firstMonth: Int = GregorianCalendarConstants.MonthsInYear*(number - 1) + 1


    def lengthInMonths = GregorianCalendarConstants.MonthsInYear


    override def character: YearCharacter = isLeap
  }


  object Year extends YearCompanion {

    override def apply(number: Int): Year = new Year(number)


    override def apply(month: Month): Year = Year((month.number - 1) / GregorianCalendarConstants.MonthsInYear +1)


    protected override def areYearsPositive: Boolean = false


    protected override def characters: Seq[YearCharacter] = Seq(true, false)


    protected override def namesAndLengths(isLeap: Boolean): List[(MonthName, Int)] = {
      List(
        (Month.January, 31),
        (Month.February, if (isLeap) 29 else 28),
        (Month.March, 31),
        (Month.April, 30),
        (Month.May, 31),
        (Month.June, 30),
        (Month.July, 31),
        (Month.August, 31),
        (Month.September, 30),
        (Month.October, 31),
        (Month.November, 30),
        (Month.December, 31)
      )
    }
  }


  final class Month(number: Int) extends MonthBase(number) {

    override def numberInYear: Int = number - year.firstMonth + 1
  }


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    case object January   extends MonthName
    case object February  extends MonthName
    case object March     extends MonthName
    case object April     extends MonthName
    case object May       extends MonthName
    case object June      extends MonthName
    case object July      extends MonthName
    case object August    extends MonthName
    case object September extends MonthName
    case object October   extends MonthName
    case object November  extends MonthName
    case object December  extends MonthName
  }


  final class Day(number: Int) extends DayBase(number) {

    def morningTime(hours: Int, parts: Int): Moment = time(Time.morningTime(hours, parts))


    def afternoonTime(hours: Int, parts: Int): Moment = time(Time.afternoonTime(hours, parts))
  }


  object Day extends DayCompanion {

    override def apply(number: Int): Day = new Day(number)


    override val FirstDayDayOfWeek = Constants.FirstDayDayOfWeekGregorian
  }


  final class Moment(days: Int, time: Time) extends MomentBase(days, time)


  object Moment extends MomentCompanion {

    override def apply(days: Int, time: Time): Moment = new Moment(days, time)
  }


  final class Time(hours: Int, parts: Int) extends TimeBase(hours, parts)


  object Time extends TimeCompanion {

    override def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def morningTime(hours: Int, parts: Int) = {
      require(hours < Constants.HoursPerHalfDay)
      Time(hours, parts)
    }


    def afternoonTime(hours: Int, parts: Int) = {
      require(hours < Constants.HoursPerHalfDay)
      Time(hours + Constants.HoursPerHalfDay, parts)
    }
  }
}
