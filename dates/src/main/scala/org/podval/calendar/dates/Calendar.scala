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


  type Moment <: MomentBase


  /**
   *
   * @param number  of the Year
   */
  protected abstract class YearBase(number: Int) extends Numbered[Year](number) { self: Year =>

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
  protected abstract class MonthBase(number: Int) extends Numbered[Month](number) { self: Month =>

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


    private[this] def descriptor = year.months(numberInYear - 1)
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



  protected final case class MonthNameAndLength(name: Month.Name, length: Int)


  protected final      class MonthDescriptor   (val name: Month.Name, val length: Int, val daysBefore: Int)


  val Month: MonthCompanion



  /**
   *
   * @param number  of the Day
   */
  protected abstract class DayBase(number: Int) extends Numbered[Day](number) { this: Day =>

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


    final def time(hours: Int = 0, minutes: Int = 0, parts: Int = 0): Moment = Moment(this, hours, minutes, parts)


    final def toFullString: String = year + " " + month.name + " " + numberInMonth
  }



  /**
   *
   */
  protected abstract class DayCompanion {

    type Name


    val daysPerWeek: Int = 7


    def names: Seq[Name]


    def apply(number: Int): Day


    final def apply(year: Int, month: Month.Name, day: Int): Day = Year(year).month(month).day(day)


    final def apply(year: Int, month: Int, day: Int): Day = Year(year).month(month).day(day)


    final def numberInWeek(dayNumber: Int): Int = ((dayNumber + firstDayNumberInWeek - 1 - 1) % daysPerWeek) + 1


    val firstDayNumberInWeek: Int
  }



  val Day: DayCompanion


  final def day(number: Int): Moment = Day(number).time()


  final def days(number: Int): Moment = Moment(days = number)


  final def hours(number: Int): Moment = days(0).hours(number)



  /**
   *
   * @param inParts
   */
  protected abstract class MomentBase(val inParts: Long) extends Ordered[Moment] {

    import Moment._

    require(inParts >= 0)


    final def days: Int = (inParts / Units.partsPerDay).toInt


    final def hours: Int = ((inParts % Units.partsPerDay) / Units.partsPerHour).toInt


    final def hours(value: Int): Moment = Moment(days, value, minutes, parts)


    final def firstHalfHours(value: Int): Moment = {
      require(0 <= hours && hours < Units.hoursPerHalfDay)
      hours(value)
    }


    final def secondHalfHours(value: Int): Moment = {
      require(0 <= value && value < Units.hoursPerHalfDay)
      hours(value + Units.hoursPerHalfDay)
    }


    final def minutes: Int = ((inParts % Units.partsPerHour) / Units.partsPerMinute).toInt


    final def minutes(value: Int): Moment = Moment(days, hours, value, parts)


    final def parts: Int = (inParts % Units.partsPerMinute).toInt


    final def parts(value: Int): Moment = Moment(days, hours, minutes, value)


    final def partsWithMinutes: Int = (inParts % Units.partsPerHour).toInt


    final def time: Moment = Moment(inParts % Units.partsPerDay)


    final override def equals(other: Any): Boolean = other match {
      case that: Moment => (inParts == that.inParts)
      case _ => false
    }


    final override def hashCode = 41 * inParts.hashCode


    final override def compare(that: Moment) = this.inParts.compare(that.inParts)


    final def +(other: Moment) = Moment(inParts + other.inParts)


    final def -(other: Moment) = Moment(inParts - other.inParts)


    final def *(n: Int): Moment = Moment(inParts * n)


    final def /(n: Int): Moment = Moment(inParts / n)


    final def %(n: Int): Moment = Moment(inParts % n)


    final def day: Day = Day(days + 1)


    final override def toString: String = days + "d" + hours + "h" + partsWithMinutes + "p"
//    final override def toString: String = days + "d" + hours + "h" + minutes + "m" + parts + "p"


    // TODO  def toFullString: String = day.toFullString + " " + time.toFullString
  }



  /**
   *
   */
  protected abstract class MomentCompanion {

    def apply(inParts: Long): Moment


    // TODO why can't I have default parameters here also?
    def apply(day: Day, hours: Int, minutes: Int, parts: Int): Moment =
      Moment(day.number - 1, hours, minutes, parts)


    def apply(days: Int = 0, hours: Int = 0, minutes: Int = 0, parts: Int = 0): Moment = {
      require(days >= 0)
      require(hours >= 0)
      require(minutes >= 0)
      require(parts >= 0)

      Moment(
        days * Units.partsPerDay.toLong +  // To force long multiplication
        hours * Units.partsPerHour +
        minutes * Units.partsPerMinute +
        parts
      )
    }
  }



  object Units {

    val hoursPerDay = 24


    require(hoursPerDay % 2 == 0)


    val hoursPerHalfDay = hoursPerDay / 2


    val partsPerHour = 1080


    val minutesPerHour = 60

    require(partsPerHour % minutesPerHour == 0)


    val partsPerDay = hoursPerDay * partsPerHour


    val partsPerMinute = partsPerHour / minutesPerHour
  }


  protected val Moment: MomentCompanion
}
