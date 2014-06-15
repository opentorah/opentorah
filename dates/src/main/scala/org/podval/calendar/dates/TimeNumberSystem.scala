package org.podval.calendar.dates

/*
 * Copyright 2014 Podval Group.
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
object TimeNumberSystem extends {

  val hoursPerDay = 24


  val partsPerHour = 1080


  val momentsPerPart = 76


  protected override val signs: List[String] = List("d", "h", "p", "m")


  protected override val ranges: List[Int] = List(hoursPerDay, partsPerHour, momentsPerPart)

} with NumberSystem {

  require(hoursPerDay % 2 == 0)


  val hoursPerHalfDay = hoursPerDay / 2


  private val minutesPerHour = 60


  require(partsPerHour % minutesPerHour == 0)


  val partsPerMinute = partsPerHour / minutesPerHour


  protected override val headRange: Option[Int] = None



  trait TimeNumberBase extends Number {

    final def days: Int = head


    final def days(value: Int): SelfType = digit(0, value)


    final def day(number: Int): SelfType = days(number-1)


    final def hours: Int = digit(1)


    final def hours(value: Int): SelfType = digit(1, value)


    final def firstHalfHours(value: Int): SelfType = {
      require(0 <= hours && hours < TimeNumberSystem.hoursPerHalfDay)
      hours(value)
    }


    final def secondHalfHours(value: Int): SelfType = {
      require(0 <= value && value < TimeNumberSystem.hoursPerHalfDay)
      hours(value + TimeNumberSystem.hoursPerHalfDay)
    }


    final def parts: Int = digit(2)


    final def parts(value: Int): SelfType = digit(2, value)


    final def minutes: Int = parts / TimeNumberSystem.partsPerMinute


    final def minutes(value: Int): SelfType = parts(value*TimeNumberSystem.partsPerMinute+partsWithoutMinutes)


    final def partsWithoutMinutes: Int = parts % TimeNumberSystem.partsPerMinute


    final def partsWithoutMinutes(value: Int): SelfType = parts(minutes*TimeNumberSystem.partsPerMinute+value)


    final def moments: Int = digit(3)


    final def moments(value: Int): SelfType = digit(3, value)
  }



  abstract class TimeNumber(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with TimeNumberBase



  final class TimeInterval(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with TimeNumberBase with ScalarNumber with Ordered[TimeInterval] {

    protected override type SelfType = TimeInterval


    protected override def selfCreator = TimeInterval.creator


    final override def compare(that: TimeInterval): Int = compare_(that)


    final override def equals(other: Any): Boolean =
      if (!other.isInstanceOf[TimeInterval]) false else equals_(other.asInstanceOf[TimeInterval])
  }


  def interval: TimeInterval = TimeInterval(false, List(0))


  object TimeInterval {

    def apply(negative: Boolean, digits: List[Int]): TimeInterval = new TimeInterval(negative, digits)


    val creator: Creator[TimeInterval] = apply _
  }
}
