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


abstract class MomentT[I <: TimeT, T <: MomentT[I, T]] protected(val days: Int, val time: I) extends Ordered[T] {

  final override def equals(other: Any): Boolean = other match {
    case that: MomentT[_,_] => (days == that.days) && (time == that.time)
    case _ => false
  }


  final override def hashCode = 41 * days.hashCode + time.hashCode


  final override def compare(that: T) = {
    val result = this.days.compare(that.days)
    if (result == 0) this.time.compare(that.time) else result
  }


  override def toString: String = (days + 1) + " " + time.toString


  def +(other: T) = normalize(
    days + other.days,
    time.hours + other.time.hours,
    time.parts + other.time.parts
  )


  def *(n: Int): T = normalize(
    days * n,
    time.hours * n,
    time.parts * n
  )


  private def normalize(days: Int, hours: Int, parts: Int): T = {
    require(0 <= days)
    require(0 <= hours)
    require(0 <= parts)

    val hours_ = hours + parts / TimeT.PartsPerHour

    create(
      days + hours_ / TimeT.HoursPerDay,
      hours_ % TimeT.HoursPerDay,
      parts % TimeT.PartsPerHour
    )
  }


  def create(days: Int, hours: Int, parts: Int): T
}
