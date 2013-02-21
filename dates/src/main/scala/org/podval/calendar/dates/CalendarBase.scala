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


trait CalendarBase {

  type Y <: YearBase


  type M <: MonthBase


  type D <: DayBase


  type O <: MomentBase


  type T <: TimeBase


  type MonthName


  trait Creator {

    def year(number: Int): Y


    def month(number: Int): M


    def day(number: Int): D


    def moment(day: Int, time: T): O


    def time(hours: Int, parts: Int): T
  }


  val creator: Creator


  final class MonthDescriptor(val name: MonthName, val length: Int, val daysBefore: Int)


  abstract class YearBase(number: Int) extends Numbered[YearBase](number) {

    final def next: Y = creator.year(number + 1)


    final def prev: Y = creator.year(number - 1)


    def isLeap: Boolean


    def firstDay: Int


    def lengthInDays: Int


    def firstMonth: Int


    def lengthInMonths: Int


    final def month(numberInYear: Int): M = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      creator.month(firstMonth + numberInYear - 1)
    }


    final def month(name: MonthName): M = month(months.indexWhere(_.name == name) + 1)


    final def month(day: D): M = {
      require(0 < day.dayOfYear && day.dayOfYear <= lengthInDays)
      month(months.count(_.daysBefore < day.dayOfYear))
    }


    def months: List[MonthDescriptor]
  }



  abstract class MonthBase(number: Int) extends Numbered[MonthBase](number) {

    def year: Y


    def numberInYear: Int


    final def day(day: Int): D = {
      require (0 < day && day <= length)
      creator.day(firstDay + day - 1)
    }


    final def firstDay: Int = year.firstDay + descriptor.daysBefore


    final def name: MonthName = descriptor.name


    final def length: Int = descriptor.length


    private def descriptor = year.months(numberInYear - 1)
  }


  abstract class DayBase(number: Int) extends Numbered[DayBase](number) {

    require(0 < number)


    final def next: D = creator.day(number + 1)


    final def prev: D = creator.day(number - 1)


    def dayOfWeek: Int


    final def dayOfMonth: Int = number - month.firstDay + 1


    final def dayOfYear: Int = number - year.firstDay + 1


    def year: Y


    /* XXX final*/ def month: M // XXX = year.month(this)


    final def time(time: T): O = creator.moment(number - 1, time)


    final def time(hours: Int, parts: Int): O = time(creator.time(hours, parts))


    final def toFullString: String = year + " " + month.name + " " + dayOfMonth
  }


  abstract class MomentBase(val days: Int, val time: T) extends Ordered[MomentBase] {

    final override def equals(other: Any): Boolean = other match {
      case that: MomentBase => (days == that.days) && (time == that.time)
      case _ => false
    }


    final override def hashCode = 41 * days.hashCode + time.hashCode


    final override def compare(that: MomentBase) = {
      val result = this.days.compare(that.days)
      if (result == 0) this.time.compare(that.time) else result
    }


    final def day: D = creator.day(days+1)


    final override def toString: String = (days + 1) + " " + time.toString


    final def toFullString: String = day.toFullString + " " + time.toFullString


    final def +(other: O) = normalize(
      days + other.days,
      time.hours + other.time.hours,
      time.parts + other.time.parts
    )


    final def *(n: Int): O = normalize(
      days * n,
      time.hours * n,
      time.parts * n
    )


    private[this] def normalize(days: Int, hours: Int, parts: Int): O = {
      require(0 <= days)
      require(0 <= hours)
      require(0 <= parts)

      val hours_ = hours + parts / TimeBase.PartsPerHour

      creator.moment(days + hours_ / TimeBase.HoursPerDay, creator.time(hours_ % TimeBase.HoursPerDay, parts % TimeBase.PartsPerHour))
    }
  }


  abstract class TimeBase(val hours: Int, val parts: Int) extends Ordered[TimeBase] {

    require(0 <= hours && hours < TimeBase.HoursPerDay)
    require(0 <= parts && parts < TimeBase.PartsPerHour)


    final override def equals(other: Any): Boolean = other match {
      case that: TimeBase => this.allParts == that.allParts
      case _ => false
    }


    final override def hashCode = 41*hours+parts


    final override def compare(that: TimeBase) = this.allParts - that.allParts


    final def isZero = (hours == 0) && (parts == 0)


    final def allParts = hours*TimeBase.PartsPerHour + parts


    final def minutes: Int = parts / TimeBase.PartsPerMinute


    final def partsOfMinute = parts % TimeBase.PartsPerMinute


    final override def toString: String = hours + "h" + parts + "p"


    final def toFullString: String = hours + ":" + minutes + ":" + partsOfMinute
  }


  object TimeBase {

    val HoursPerDay = 24


    require(HoursPerDay % 2 == 0)


    val HoursPerHalfDay = HoursPerDay / 2


    val PartsPerHour = 1080


    val MinutesPerHour = 60


    require(PartsPerHour % MinutesPerHour == 0)


    val PartsPerMinute = PartsPerHour / MinutesPerHour
  }
}
