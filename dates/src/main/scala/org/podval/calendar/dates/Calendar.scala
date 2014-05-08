/*
 * Copyright 2011-2014 Podval Group.
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


abstract class Calendar {

  type Year <: YearBase


  type Month <: MonthBase


  type Day <: DayBase


  protected val helper: Helper


  abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

    final def next: Year = yearCompanion(number + 1)


    final def prev: Year = yearCompanion(number - 1)


    def firstDay: Int


    def lengthInDays: Int


    final def firstMonth: Int = helper.firstMonth(number)


    final def lengthInMonths: Int = helper.lengthInMonths(number)


    def character: yearCompanion.Character


    final def isLeap: Boolean = helper.isLeap(number)


    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      monthCompanion(firstMonth + numberInYear - 1)
    }

    final def month(name: monthCompanion.Name): Month = month(months.indexWhere(_.name == name) + 1)


    final def monthForDay(day: Int) = {
      require(0 < day && day <= lengthInDays)
      month(months.count(_.daysBefore < day))
    }

    final def months: List[monthCompanion.Descriptor] = yearCompanion.months(this.character)
  }


  protected abstract class YearCompanion {

    type Character


    def apply(number: Int): Year


    final  def apply(month: Month): Year = apply(helper.yearNumber(month.number))


    final def apply(day: Day): Year = {
      var result = apply(helper.yearForSureBefore(day.number))
      require(result.firstDay <= day.number)
      while (result.next.firstDay <= day.number) result = result.next
      result
    }


    val months: Map[yearCompanion.Character, List[monthCompanion.Descriptor]] =
      Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)


    protected def characters: Seq[yearCompanion.Character]


    private def monthsGenerator(character: yearCompanion.Character): List[monthCompanion.Descriptor] = {
      val namesAndLengths = this.namesAndLengths(character)
      val (_, lengths) = namesAndLengths.unzip
      val daysBefore = lengths.scanLeft(0)(_ + _).init
      (namesAndLengths zip daysBefore) map (m => new monthCompanion.Descriptor(m._1._1, m._1._2, m._2))
    }


    protected def namesAndLengths(character: yearCompanion.Character): List[(monthCompanion.Name, Int)]
  }


  protected val yearCompanion: YearCompanion


  abstract class MonthBase(number: Int) extends Numbered[Month](number) { self: Month =>

    require(0 < number)


    final def next: Month = monthCompanion(number + 1)


    final def prev: Month = monthCompanion(number - 1)


    final def year: Year = yearCompanion(this)


    final def numberInYear: Int = helper.numberInYear(number)


    final def day(day: Int): Day = {
      require (0 < day && day <= length)
      dayCompanion(firstDay + day - 1)
    }


    final def firstDay: Int = year.firstDay + descriptor.daysBefore


    final def name: monthCompanion.Name = descriptor.name


    final def length: Int = descriptor.length


    private def descriptor = year.months(numberInYear - 1)
  }


  protected abstract class MonthCompanion {

    type Name


    final class Descriptor(val name: monthCompanion.Name, val length: Int, val daysBefore: Int)


    def apply(number: Int): Month


    final def apply(year: Int, monthInYear: Int): Month = yearCompanion(year).month(monthInYear)
  }


  protected val monthCompanion: MonthCompanion



  abstract class DayBase(number: Int) extends Numbered[Day](number) { this: Day =>

    require(0 < number)


    final def next: Day = dayCompanion(number + 1)


    final def prev: Day = dayCompanion(number - 1)


    final def +(change: Int) = dayCompanion(number + change)


    final def -(change: Int) = dayCompanion(number - change)


    final def year: Year = yearCompanion(this)


    final def month: Month = year.monthForDay(numberInYear)


    final def numberInYear: Int = number - year.firstDay + 1


    final def numberInMonth: Int = number - month.firstDay + 1


    final def numberInWeek: Int = helper.numberInWeek(number)


    final def name: dayCompanion.Name = dayCompanion.names(numberInWeek - 1)


    final def time(time: Time): Moment = momentCompanion(number - 1, time)


    final def time(hours: Int, parts: Int): Moment = time(timeCompanion(hours, parts))


    final def time(hours: Int, minutes: Int, parts: Int): Moment = time(timeCompanion(hours, minutes, parts))


    final def toFullString: String = year + " " + month.name + " " + numberInMonth
  }


  protected abstract class DayCompanion {

    type Name


    def names: Seq[dayCompanion.Name]


    def apply(number: Int): Day


    final def apply(year: Int, month: monthCompanion.Name, day: Int): Day = yearCompanion(year).month(month).day(day)


    final def apply(year: Int, month: Int, day: Int): Day = yearCompanion(year).month(month).day(day)
  }


  protected val dayCompanion: DayCompanion


  final class Moment(days: Int, time: Time) extends MomentBase[Time, Moment](days, time) { self: Moment =>

    def day: Day = dayCompanion(days + 1)


    override def toString: String = day + " " + time


    def toFullString: String = day.toFullString + " " + time.toFullString


    protected override def create(days: Int, hours: Int, parts: Int): Moment = momentCompanion(days, timeCompanion(hours, parts))
  }


  protected abstract class MomentCompanion {

    def apply(days: Int, time: Time): Moment
  }


  protected val momentCompanion: MomentCompanion


  final class Time(hours: Int, parts: Int) extends TimeBase[Time](hours, parts)


  protected abstract class TimeCompanion {

    def apply(hours: Int, parts: Int): Time


    final def apply(hours: Int, minutes: Int, parts: Int): Time = apply(hours, minutes * Helper.partsPerMinute + parts)
  }


  protected val timeCompanion: TimeCompanion
}
