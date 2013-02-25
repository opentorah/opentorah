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


  // XXX submerge in Year
  type YearCharacter


  // XXX submerge
  final class MonthDescriptor(val name: monthCompanion.Name, val length: Int, val daysBefore: Int)


  protected val helper: CalendarHelper


  abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

    final def next: Year = yearCompanion(number + 1)


    final def prev: Year = yearCompanion(number - 1)


    def firstDay: Int


    def lengthInDays: Int


    final def firstMonth: Int = helper.firstMonth(number)


    def lengthInMonths: Int = helper.lengthInMonths(number)


    def character: YearCharacter


    def isLeap: Boolean = helper.isLeap(number)


    final def month(numberInYear /* XXX name monthOfYear, like others? */: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      monthCompanion(firstMonth + numberInYear - 1)
    }


    final def month(name: monthCompanion.Name): Month = month(months.indexWhere(_.name == name) + 1)


    final def month(day: Day): Month = {
      require(0 < day.dayOfYear && day.dayOfYear <= lengthInDays)
      month(months.count(_.daysBefore < day.dayOfYear))
    }


    final def months: List[MonthDescriptor] = yearCompanion.Months(this.character)
  }


  protected abstract class YearCompanion {

    def apply(number: Int): Year


    final  def apply(month: Month): Year = apply(helper.yearNumberOfMonth(month.number))


    final def apply(day: Day): Year = {
      // XXX give names to constants
      val startingYear = (4 * day.number / (4 * 365 + 1)) - 1
      var result = apply(if (areYearsPositive) scala.math.max(1, startingYear) else startingYear)
      require(result.firstDay <= day.number)
      while (result.next.firstDay <= day.number) result = result.next
      result
    }


    protected def areYearsPositive: Boolean


    val Months: Map[YearCharacter, List[MonthDescriptor]] =
      Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)


    protected def characters: Seq[YearCharacter]


    private def monthsGenerator(character: YearCharacter): List[MonthDescriptor] = {
      val namesAndLengths = this.namesAndLengths(character)
      val (_, lengths) = namesAndLengths.unzip
      val daysBefore = lengths.scanLeft(0)(_ + _).init
      (namesAndLengths zip daysBefore) map (m => new MonthDescriptor(m._1._1, m._1._2, m._2))
    }


    protected def namesAndLengths(character: YearCharacter): List[(monthCompanion.Name, Int)]
  }


  protected val yearCompanion: YearCompanion


  abstract class MonthBase(number: Int) extends Numbered[Month](number) { self: Month =>

    require(0 < number)


    final def next: Month = monthCompanion(number + 1)


    final def prev: Month = monthCompanion(number - 1)


    final def year: Year = yearCompanion(this)


    final def numberInYear: Int = helper.numberInYearOfMonth(number)


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


    def apply(number: Int): Month


    def apply(year: Int, monthInYear: Int): Month = yearCompanion(year).month(monthInYear)
  }


  protected val monthCompanion: MonthCompanion


  abstract class DayBase(number: Int) extends Numbered[Day](number) { this: Day =>

    require(0 < number)


    final def next: Day = dayCompanion(number + 1)


    final def prev: Day = dayCompanion(number - 1)


    final def dayOfWeek: Int = helper.dayOfWeek(number)


    final def dayOfMonth: Int = number - month.firstDay + 1


    final def dayOfYear: Int = number - year.firstDay + 1


    final def year: Year = yearCompanion(this)


    final def month: Month = year.month(this)


    final def time(time: Time): Moment = momentCompanion(number - 1, time)


    final def time(hours: Int, parts: Int): Moment = time(timeCompanion(hours, parts))


    final def toFullString: String = year + " " + month.name + " " + dayOfMonth
  }


  protected abstract class DayCompanion {

    def apply(number: Int): Day


    def apply(year: Int, month: monthCompanion.Name, day: Int): Day = yearCompanion(year).month(month).day(day)
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


    final def day: Day = dayCompanion(days+1)


    final override def toString: String = day + " " + time.toString


    final def toFullString: String = day.toFullString + " " + time.toFullString


    final def +(other: Moment) = normalize(
      days + other.days,
      time.hours + other.time.hours,
      time.parts + other.time.parts
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

      val hours_ = hours + parts / CalendarHelper.partsPerHour

      momentCompanion(
        days + hours_ / CalendarHelper.hoursPerDay,
        timeCompanion(
          hours_ % CalendarHelper.hoursPerDay,
          parts % CalendarHelper.partsPerHour))
    }
  }


  protected abstract class MomentCompanion {

    def apply(days: Int, time: Time): Moment
  }


  protected val momentCompanion: MomentCompanion


  abstract class TimeBase(val hours: Int, val parts: Int) extends Ordered[Time] { self: Time =>

    require(0 <= hours && hours < CalendarHelper.hoursPerDay)
    require(0 <= parts && parts < CalendarHelper.partsPerHour)


    final override def equals(other: Any): Boolean = other match {
      case that: Time => this.allParts == that.allParts
      case _ => false
    }


    final override def hashCode = 41*hours+parts


    final override def compare(that: Time) = this.allParts - that.allParts


    final def isZero = (hours == 0) && (parts == 0)


    final def allParts /* XXX toParts? asParts? */ = hours * CalendarHelper.partsPerHour + parts


    final def minutes: Int = parts / CalendarHelper.partsPerMinute


    final def partsOfMinute = parts % CalendarHelper.partsPerMinute


    final override def toString: String = hours + "h" + parts + "p"


    final def toFullString: String = hours + ":" + minutes + ":" + partsOfMinute
  }


  protected abstract class TimeCompanion {

    def apply(hours: Int, parts: Int): Time
  }


  protected val timeCompanion: TimeCompanion
}
