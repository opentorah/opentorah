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


abstract class MomentBase[Time <: TimeBase[Time], Moment <: MomentBase[Time, Moment]](val days: Int, val time: Time)
  extends Ordered[Moment] { self: Moment =>

  final override def equals(other: Any): Boolean = other match {
    case that: Moment => (days == that.days) && (time == that.time)
    case _ => false
  }


  final override def hashCode = 41 * days.hashCode + time.hashCode


  final override def compare(that: Moment) = {
    val result = this.days.compare(that.days)
    if (result == 0) this.time.compare(that.time) else result
  }


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
    val daysN = days + hours_ / Helper.hoursPerDay
    val hoursN = hours_ % Helper.hoursPerDay
    val partsN = parts % Helper.partsPerHour

    create(daysN, hoursN, partsN)
  }


  protected def create(days: Int, hours: Int, parts: Int): Moment
}
