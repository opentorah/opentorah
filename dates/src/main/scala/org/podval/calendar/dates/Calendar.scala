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


abstract class Calendar {

  // Instead of "Y" here and type Y = Year in the sub-trait, I use "Year" here and just define "Year" in the sub-trate. Beaty!

  type Year <: YearBase


  type Month <: MonthBase


  type Day <: DayBase


  type Moment <: MomentBase


  type Time <: TimeBase


  protected val helper: Helper


  abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

    final def next: Year = yearCompanion(number + 1)


    final def prev: Year = yearCompanion(number - 1)


    def firstDay: Int


    def lengthInDays: Int


    final def firstMonth: Int = helper.firstMonth(number)


    def lengthInMonths: Int = helper.lengthInMonths(number)


    def character: yearCompanion.Character


    def isLeap: Boolean = helper.isLeap(number)


    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      monthCompanion(firstMonth + numberInYear - 1)
    }

    final def monthForDay(day: Int) = {
      require(0 < day && day <= lengthInDays)
      month(months.count(_.daysBefore < day))
    }

    final def month(name: monthCompanion.Name): Month = month(months.indexWhere(_.name == name) + 1)


    final def months: List[monthCompanion.Descriptor] = yearCompanion.Months(this.character)
  }


  protected abstract class YearCompanion {

    type Character


    def apply(number: Int): Year


    final  def apply(month: Month): Year = apply(helper.yearNumber(month.number))


    final def apply(day: Day): Year = {
      val yearForSureBefore = Helper.yearForSureBefore(day.number)
      var result = apply(if (areYearsPositive) scala.math.max(1, yearForSureBefore) else yearForSureBefore)
      require(result.firstDay <= day.number)
      while (result.next.firstDay <= day.number) result = result.next
      result
    }


    protected def areYearsPositive: Boolean


    val Months: Map[yearCompanion.Character, List[monthCompanion.Descriptor]] =
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



  // XXX Introduce methods common for Day and Moment?
  private trait DayLike {
//
//    final def year: Year = yearCompanion(this)
//
//
//    final def month: Month = year.monthForDay(numberInYear)
//
//
//    final def numberInYear: Int = number - year.firstDay + 1
//
//
//    final def numberInMonth: Int = number - month.firstDay + 1
//
//
//    final def numberInWeek: Int = helper.numberInWeek(number)
//
//
//    final def name: dayCompanion.Name = dayCompanion.names(numberInWeek - 1)
  }


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


  abstract class MomentBase(val days: Int, val time: Time) extends Ordered[Moment] { self: Moment =>

    final override def equals(other: Any): Boolean = other match {
      case that: Moment => (days == that.days) && (time == that.time)
      case _ => false
    }


    final override def hashCode = 41 * days.hashCode + time.hashCode


    final override def compare(that: Moment) = {
      val result = this.days.compare(that.days)
      if (result == 0) this.time.compare(that.time) else result
    }


    final def day: Day = dayCompanion(days + 1)


    final override def toString: String = day + " " + time.toString


    final def toFullString: String = day.toFullString + " " + time.toFullString


    final def +(other: Moment) = normalize(
      days + other.days,
      time.hours + other.time.hours,
      time.parts + other.time.parts
    )


    final def -(other: Moment) = normalize(
      days - other.days,
      time.hours - other.time.hours,
      time.parts - other.time.parts
    )


    final def *(n: Int): Moment = normalize(
      days * n,
      time.hours * n,
      time.parts * n
    )


    private[this] def normalize(days: Int, hours: Int, parts: Int): Moment = {
      require(0 <= days)
      require(0 <= hours)
      require(0 <= parts)

      val hours_ = hours + parts / Helper.partsPerHour

      momentCompanion(
        days + hours_ / Helper.hoursPerDay,
        timeCompanion(
          hours_ % Helper.hoursPerDay,
          parts % Helper.partsPerHour))
    }
  }


  protected abstract class MomentCompanion {

    def apply(days: Int, time: Time): Moment
  }


  protected val momentCompanion: MomentCompanion


  abstract class TimeBase(val hours: Int, val parts: Int) extends Ordered[Time] { self: Time =>

    require(0 <= hours && hours < Helper.hoursPerDay)
    require(0 <= parts && parts < Helper.partsPerHour)


    final override def equals(other: Any): Boolean = other match {
      case that: Time => this.allParts == that.allParts
      case _ => false
    }


    final override def hashCode = 41 * hours + parts


    final override def compare(that: Time) = this.allParts - that.allParts


    final def isZero = (hours == 0) && (parts == 0)


    final def allParts /* XXX toParts? asParts? */ = hours * Helper.partsPerHour + parts


    final def minutes: Int = parts / Helper.partsPerMinute


    final def partsOfMinute = parts % Helper.partsPerMinute


    final override def toString: String = hours + "h" + parts + "p"


    final def toFullString: String = hours + ":" + minutes + ":" + partsOfMinute
  }


  protected abstract class TimeCompanion {

    def apply(hours: Int, parts: Int): Time


    final def apply(hours: Int, minutes: Int, parts: Int): Time = apply(hours, minutes * Helper.partsPerMinute + parts)
  }


  protected val timeCompanion: TimeCompanion
}
