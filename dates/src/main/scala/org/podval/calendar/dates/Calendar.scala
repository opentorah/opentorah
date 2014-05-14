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


/* TODO
  There are things that I do not yet understand about Scala's approach to family polymorphism.

  o Is this a "cake"?
  o Is there any way to regain the ability to split the code into files?

  o When I put MonthDescriptor inside Mont companion, types do not match; when it is outside, they do, although it
    references a type from the companion (Month.Name).

  o In derived Calendars, many companion vals are overridden by objects, but "override object" is not legal Scala.

  o If Year is done as class and an instance assigned to the overridden val, it works; if it is done as an
    override object, I get compiler errors:

        overriding method character in class YearBase of type => org.podval.calendar.dates.Jewish.Year.Character;
        method character has incompatible type
          override def character: Year.Character = (isLeap, kind)

  o Derived Calendars are objects, but unless I do things like val x = Jewish, I used to get initialization errors!
    Which now went away for some reason! Maybe, because I took MonthDescriptor out of the Month companion!
 */
abstract class Calendar {

  type Year <: YearBase


  type Month <: MonthBase


  type Day <: DayBase



  /**
   *
   * @param number  of the Year
   */
  abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

    final def next: Year = Year(number + 1)


    final def prev: Year = Year(number - 1)


    def firstDay: Int


    def lengthInDays: Int


    final def firstMonth: Int = Year.firstMonth(number)


    final def lengthInMonths: Int = Year.lengthInMonths(number)


    def character: Year.Character


    final def isLeap: Boolean = Year.isLeap(number)


    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      Month(firstMonth + numberInYear - 1)
    }


    final def month(name: Month.Name): Month = month(months.indexWhere(_.name == name) + 1)


    final def monthForDay(day: Int) = {
      require(0 < day && day <= lengthInDays)
      month(months.count(_.daysBefore < day))
    }

    final def months: List[MonthDescriptor] = Year.months(character)
  }



  /**
   *
   */
  protected abstract class YearCompanionBase {

    type Character


    def apply(number: Int): Year


    final  def apply(month: Month): Year = apply(Month.yearNumber(month.number))


    final def apply(day: Day): Year = {
      var result = apply(yearsForSureBefore(day.number))
      require(result.firstDay <= day.number)
      while (result.next.firstDay <= day.number) result = result.next
      result
    }


    val months: Map[Year.Character, List[MonthDescriptor]] =
      Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)


    protected def characters: Seq[Year.Character]


    private[this] def monthsGenerator(character: Year.Character): List[MonthDescriptor] = {
      val namesAndLengths = monthNamesAndLengths(character)
      val daysBefore = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
      namesAndLengths zip daysBefore map { case (nameAndLength, daysBefore) =>
        new MonthDescriptor(nameAndLength.name, nameAndLength.length, daysBefore)
      }
    }


    protected def monthNamesAndLengths(character: Year.Character): List[MonthNameAndLength]


    protected def areYearsPositive: Boolean


    private[this] final def yearsForSureBefore(dayNumber: Int): Int =  {
      val result = (4 * dayNumber / (4 * 365 + 1)) - 1
      if (areYearsPositive) scala.math.max(1, result) else result
    }


    def isLeap(yearNumber: Int): Boolean


    def firstMonth(yearNumber: Int): Int


    def lengthInMonths(yearNumber: Int): Int
  }


  val Year: YearCompanionBase



  /**
   *
   * @param number  of the Month
   */
  abstract class MonthBase(number: Int) extends Numbered[Month](number) { self: Month =>

    require(0 < number)


    final def next: Month = Month(number + 1)


    final def prev: Month = Month(number - 1)


    final def year: Year = Year(this)


    final def numberInYear: Int = Month.numberInYear(number)


    final def day(day: Int): Day = {
      require (0 < day && day <= length)
      Day(firstDay + day - 1)
    }


    final def firstDay: Int = year.firstDay + descriptor.daysBefore


    final def name: Month.Name = descriptor.name


    final def length: Int = descriptor.length


    private def descriptor = year.months(numberInYear - 1)
  }



  /**
   *
   */
  protected abstract class MonthCompanion {

    type Name


    def apply(number: Int): Month


    final def apply(year: Int, monthInYear: Int): Month = Year(year).month(monthInYear)


    def yearNumber(monthNumber: Int): Int


    def numberInYear(monthNumber: Int): Int
  }



  final case class MonthNameAndLength(name: Month.Name, length: Int)


  final      class MonthDescriptor   (val name: Month.Name, val length: Int, val daysBefore: Int)


  val Month: MonthCompanion



  /**
   *
   * @param number  of the Day
   */
  abstract class DayBase(number: Int) extends Numbered[Day](number) { this: Day =>

    require(0 < number)


    final def next: Day = Day(number + 1)


    final def prev: Day = Day(number - 1)


    final def +(change: Int) = Day(number + change)


    final def -(change: Int) = Day(number - change)


    final def year: Year = Year(this)


    final def month: Month = year.monthForDay(numberInYear)


    final def numberInYear: Int = number - year.firstDay + 1


    final def numberInMonth: Int = number - month.firstDay + 1


    final def numberInWeek: Int = Day.numberInWeek(number)


    final def name: Day.Name = Day.names(numberInWeek - 1)


    final def time(time: Time): Moment = Moment(number - 1, time)  // TODO make intervals and this -1 more intuitive!


    // TODO replace with more obvious .hours().parts()
    final def time(hours: Int, minutes: Int, parts: Int): Moment = time(Time(hours, minutes, parts))


    final def toFullString: String = year + " " + month.name + " " + numberInMonth
  }



  /**
   *
   */
  protected abstract class DayCompanion {

    type Name


    def names: Seq[Name]


    def apply(number: Int): Day


    final def apply(year: Int, month: Month.Name, day: Int): Day = Year(year).month(month).day(day)


    final def apply(year: Int, month: Int, day: Int): Day = Year(year).month(month).day(day)


    final def numberInWeek(dayNumber: Int): Int = ((dayNumber + firstDayNumberInWeek - 1 - 1) % Calendar.daysPerWeek) + 1


    val firstDayNumberInWeek: Int
  }


  val Day: DayCompanion



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


    def asParts = days * Calendar.partsPerDay + time.asParts


    def +(other: Moment) = Moment(
      days + other.days,
      time.hours + other.time.hours,
      time.parts + other.time.parts
    )


    def -(other: Moment) = Moment(
      days - other.days,
      time.hours - other.time.hours,
      time.parts - other.time.parts
    )


    def *(n: Int): Moment = Moment(
      days * n,
      time.hours * n,
      time.parts * n
    )


    def /(n: Int): Moment = Moment(0, 0, asParts / n)


    def day: Day = Day(days + 1)


    override def toString: String = day + " " + time


    def toFullString: String = day.toFullString + " " + time.toFullString
  }



  /**
   *
   */
  protected abstract class MomentCompanion {

    def apply(days: Int, time: Time): Moment


    def apply(day: Day, hours: Int, minutes: Int, parts: Int): Moment = Moment(day.number - 1, hours, minutes, parts)


    def apply(day: Day, hours: Int, parts: Int): Moment = Moment(day.number - 1, hours, parts)


    def apply(days: Int, hours: Int, minutes: Int, parts: Int): Moment = Moment(days, hours, minutes * Calendar.partsPerMinute + parts)


    def apply(days: Int, hours: Int = 0, parts: Int = 0): Moment = {
      require(0 <= days)
      require(0 <= hours)
      require(0 <= parts)

      val hours_ = hours + parts / Calendar.partsPerHour
      val daysN = days + hours_ / Calendar.hoursPerDay
      val hoursN = hours_ % Calendar.hoursPerDay
      val partsN = parts % Calendar.partsPerHour

      Moment(daysN, Time(hoursN, partsN))
    }
  }


  protected val Moment: MomentCompanion



  /**
   *
   * @param hours  till this Time
   * @param parts  of the hour till this Time
   */
  // TODO dissolve?
  final class Time(val hours: Int, val parts: Int) extends Ordered[Time] {

    require(0 <= hours && hours < Calendar.hoursPerDay)
    require(0 <= parts && parts < Calendar.partsPerHour)


    override def equals(other: Any): Boolean = other match {
      case that: Time => this.asParts == that.asParts
      case _ => false
    }


    override def hashCode = 41 * hours + parts


    override def compare(that: Time) = this.asParts - that.asParts


    def isZero = (hours == 0) && (parts == 0)


    def asParts = hours * Calendar.partsPerHour + parts


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


    final def apply(hours: Int, minutes: Int, parts: Int): Time = Time(hours, minutes * Calendar.partsPerMinute + parts)
  }


  protected val Time: TimeCompanion
}




object Calendar {

    val hoursPerDay = 24

    require(hoursPerDay % 2 == 0)


    val hoursPerHalfDay = hoursPerDay / 2


    val partsPerHour = 1080


    val minutesPerHour = 60

    require(partsPerHour % minutesPerHour == 0)


    val partsPerMinute = partsPerHour / minutesPerHour


    val partsPerDay = hoursPerDay * partsPerHour

    val daysPerWeek: Int = 7
}
