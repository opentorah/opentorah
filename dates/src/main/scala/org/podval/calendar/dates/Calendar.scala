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



  /**
   *
   * @param number  of the Year
   */
  abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

    final def next: Year = yearCompanion(number + 1)


    final def prev: Year = yearCompanion(number - 1)


    def firstDay: Int


    def lengthInDays: Int


    final def firstMonth: Int = yearCompanion.firstMonth(number)


    final def lengthInMonths: Int = yearCompanion.lengthInMonths(number)


    def character: yearCompanion.Character


    final def isLeap: Boolean = yearCompanion.isLeap(number)


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



  /**
   *
   */
  protected abstract class YearCompanion {

    type Character


    def apply(number: Int): Year


    final  def apply(month: Month): Year = apply(monthCompanion.yearNumber(month.number))


    final def apply(day: Day): Year = {
      var result = apply(yearForSureBefore(day.number))
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


    protected def areYearsPositive: Boolean


    // TODO give names to constants
    final def yearForSureBefore(dayNumber: Int): Int =  {
      val result = (4 * dayNumber / (4 * 365 + 1)) - 1
      if (areYearsPositive) scala.math.max(1, result) else result
    }


    def isLeap(yearNumber: Int): Boolean


    def firstMonth(yearNumber: Int): Int


    def lengthInMonths(yearNumber: Int): Int
  }


  protected val yearCompanion: YearCompanion



  /**
   *
   * @param number  of the Month
   */
  abstract class MonthBase(number: Int) extends Numbered[Month](number) { self: Month =>

    require(0 < number)


    final def next: Month = monthCompanion(number + 1)


    final def prev: Month = monthCompanion(number - 1)


    final def year: Year = yearCompanion(this)


    final def numberInYear: Int = monthCompanion.numberInYear(number)


    final def day(day: Int): Day = {
      require (0 < day && day <= length)
      dayCompanion(firstDay + day - 1)
    }


    final def firstDay: Int = year.firstDay + descriptor.daysBefore


    final def name: monthCompanion.Name = descriptor.name


    final def length: Int = descriptor.length


    private def descriptor = year.months(numberInYear - 1)
  }



  /**
   *
   */
  protected abstract class MonthCompanion {

    type Name


    final class Descriptor(val name: monthCompanion.Name, val length: Int, val daysBefore: Int)


    def apply(number: Int): Month


    final def apply(year: Int, monthInYear: Int): Month = yearCompanion(year).month(monthInYear)


    def yearNumber(monthNumber: Int): Int


    def numberInYear(monthNumber: Int): Int
  }


  protected val monthCompanion: MonthCompanion



  /**
   *
   * @param number  of the Day
   */
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


    final def numberInWeek: Int = dayCompanion.numberInWeek(number)


    final def name: dayCompanion.Name = dayCompanion.names(numberInWeek - 1)


    final def time(time: Time): Moment = momentCompanion(number - 1, time)


    final def time(hours: Int, parts: Int): Moment = time(timeCompanion(hours, parts))


    final def time(hours: Int, minutes: Int, parts: Int): Moment = time(timeCompanion(hours, minutes, parts))


    final def toFullString: String = year + " " + month.name + " " + numberInMonth
  }



  /**
   *
   */
  protected abstract class DayCompanion {

    type Name


    def names: Seq[dayCompanion.Name]


    def apply(number: Int): Day


    final def apply(year: Int, month: monthCompanion.Name, day: Int): Day = yearCompanion(year).month(month).day(day)


    final def apply(year: Int, month: Int, day: Int): Day = yearCompanion(year).month(month).day(day)


    final def numberInWeek(dayNumber: Int): Int = ((dayNumber + firstDayNumberInWeek - 1 - 1) % Calendar.daysPerWeek) + 1


    val firstDayNumberInWeek: Int
  }


  protected val dayCompanion: DayCompanion



  /**
   *
   * @param days  till this Moment
   * @param time  of the day
   */
  final class Moment(val days: Int, val time: Time) extends Ordered[Moment] {

    override def equals(other: Any): Boolean = other match {
      case that: Moment => (days == that.days) && (time == that.time)
      case _ => false
    }


    override def hashCode = 41 * days.hashCode + time.hashCode


    override def compare(that: Moment) = {
      val result = this.days.compare(that.days)
      if (result == 0) this.time.compare(that.time) else result
    }


    def +(other: Moment) = normalize(
      days + other.days,
      time.hours + other.time.hours,
      time.parts + other.time.parts
    )


    def -(other: Moment) = normalize(
      days - other.days,
      time.hours - other.time.hours,
      time.parts - other.time.parts
    )


    def *(n: Int): Moment = normalize(
      days * n,
      time.hours * n,
      time.parts * n
    )


    private[this] def normalize(days: Int, hours: Int, parts: Int): Moment = {
      require(0 <= days)
      require(0 <= hours)
      require(0 <= parts)

      val hours_ = hours + parts / Calendar.partsPerHour
      val daysN = days + hours_ / Calendar.hoursPerDay
      val hoursN = hours_ % Calendar.hoursPerDay
      val partsN = parts % Calendar.partsPerHour

      create(daysN, hoursN, partsN)
    }


    protected def create(days: Int, hours: Int, parts: Int): Moment = momentCompanion(days, timeCompanion(hours, parts))


    def day: Day = dayCompanion(days + 1)


    override def toString: String = day + " " + time


    def toFullString: String = day.toFullString + " " + time.toFullString
  }



  /**
   *
   */
  protected abstract class MomentCompanion {

    def apply(days: Int, time: Time): Moment
  }


  protected val momentCompanion: MomentCompanion



  /**
   *
   * @param hours  till this Time
   * @param parts  of the hour till this Time
   */
  final class Time(val hours: Int, val parts: Int) extends Ordered[Time] {

    require(0 <= hours && hours < Calendar.hoursPerDay)
    require(0 <= parts && parts < Calendar.partsPerHour)


    override def equals(other: Any): Boolean = other match {
      case that: Time => this.allParts == that.allParts
      case _ => false
    }


    override def hashCode = 41 * hours + parts


    override def compare(that: Time) = this.allParts - that.allParts


    def isZero = (hours == 0) && (parts == 0)


    def allParts /* TODO toParts? asParts? */ = hours * Calendar.partsPerHour + parts


    def minutes: Int = parts / Calendar.partsPerMinute


    def partsOfMinute = parts % Calendar.partsPerMinute


    override def toString: String = hours + "h" + parts + "p"


    def toFullString: String = hours + ":" + minutes + ":" + partsOfMinute
  }



  /**
   *
   */
  protected abstract class TimeCompanion {

    def apply(hours: Int, parts: Int): Time


    final def apply(hours: Int, minutes: Int, parts: Int): Time = apply(hours, minutes * Calendar.partsPerMinute + parts)
  }


  protected val timeCompanion: TimeCompanion
}




object Calendar {
    val hoursPerDay = 24

    require(hoursPerDay % 2 == 0)


    val hoursPerHalfDay = hoursPerDay / 2


    val partsPerHour = 1080


    val minutesPerHour = 60

    require(partsPerHour % minutesPerHour == 0)


    val partsPerMinute = partsPerHour / minutesPerHour


    val daysPerWeek: Int = 7
}
