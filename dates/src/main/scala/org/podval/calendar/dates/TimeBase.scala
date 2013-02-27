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


abstract class TimeBase[Time <: TimeBase[Time]](val hours: Int, val parts: Int) extends Ordered[Time] { self: Time =>

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
