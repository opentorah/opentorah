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


object Gregorian extends Calendar {

  // XXX assignments of the companion objects have to happen early on, but even this is not sufficient!
  // I found that I need to assign JewishCalendar to a val to trigger its initialization - or I end up with a null for the Year companion object!
  override protected val timeCompanion = Time
  override protected val momentCompanion = Moment
  override protected val dayCompanion = Day
  override protected val monthCompanion = Month
  override protected val yearCompanion = Year


  protected override val helper: GregorianHelper.type = GregorianHelper


  final class Year(number: Int) extends YearBase(number) {

    override def firstDay: Int = helper.firstDay(number)


    override def lengthInDays = helper.lengthInDays(number)


    override def character: yearCompanion.Character = isLeap
  }


  object Year extends YearCompanion {

    type Character = Boolean


    override def apply(number: Int): Year = new Year(number)


    protected override def areYearsPositive: Boolean = false


    protected override def characters: Seq[yearCompanion.Character] = Seq(true, false)


    protected override def namesAndLengths(isLeap: Boolean): List[(Month.Name, Int)] = {
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


  final class Month(number: Int) extends MonthBase(number)


  object Month extends MonthCompanion {

    override def apply(number: Int): Month = new Month(number)


    sealed class Name(name: String) {

      final override def toString: String = name
    }

    case object January   extends Name("January")
    case object February  extends Name("February")
    case object March     extends Name("March")
    case object April     extends Name("April")
    case object May       extends Name("May")
    case object June      extends Name("June")
    case object July      extends Name("July")
    case object August    extends Name("August")
    case object September extends Name("September")
    case object October   extends Name("October")
    case object November  extends Name("November")
    case object December  extends Name("December")
  }


  final class Day(number: Int) extends DayBase(number) {

    def morningTime(hours: Int, parts: Int): Moment = time(Time.morningTime(hours, parts))


    def afternoonTime(hours: Int, parts: Int): Moment = time(Time.afternoonTime(hours, parts))
  }


  object Day extends DayCompanion {

    sealed class Name(name: String) {

      final override def toString: String = name
    }

    case object Sunday    extends Name("Sunday")
    case object Monday    extends Name("Monday")
    case object Tuesday   extends Name("Tuesday")
    case object Wednesday extends Name("Wednesday")
    case object Thursday  extends Name("Thursday")
    case object Friday    extends Name("Friday")
    case object Saturday  extends Name("Saturday")


    def names: Seq[dayCompanion.Name] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)


    override def apply(number: Int): Day = new Day(number)
  }


  final class Moment(days: Int, time: Time) extends MomentBase(days, time)


  object Moment extends MomentCompanion {

    override def apply(days: Int, time: Time): Moment = new Moment(days, time)
  }


  final class Time(hours: Int, parts: Int) extends TimeBase(hours, parts)


  object Time extends TimeCompanion {

    override def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def morningTime(hours: Int, parts: Int) = {
      require(hours < Helper.hoursPerHalfDay)
      Time(hours, parts)
    }


    def afternoonTime(hours: Int, parts: Int) = {
      require(hours < Helper.hoursPerHalfDay)
      Time(hours + Helper.hoursPerHalfDay, parts)
    }
  }
}
