/*
 * Copyright 2011 Podval Group.
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


class TimeT protected (val hours: Int, val parts: Int) extends Ordered[TimeT] {

    require(0 <= hours && hours < TimeT.HoursPerDay)
    require(0 <= parts && parts < TimeT.PartsPerHour)


    final override def equals(other: Any): Boolean = other match {
        case that: TimeT => this.allParts == that.allParts
        case _ => false
    }


    final override def hashCode = 41*hours+parts


    final override def compare(that: TimeT) = this.allParts - that.allParts


    final def isZero = (hours == 0) && (parts == 0)


    final def allParts = hours*TimeT.PartsPerHour + parts


    final def minutes: Int = parts / TimeT.PartsPerMinute


    final def partsOfMinute = parts % TimeT.PartsPerMinute


    final override def toString: String = hours + "h" + parts + "p"


    final def toFullString: String = hours + ":" + minutes + ":" + partsOfMinute
}


object TimeT {

    val HoursPerDay = 24


    require(HoursPerDay % 2 == 0)


    val HoursPerHalfDay = HoursPerDay / 2


    val PartsPerHour = 1080


    val MinutesPerHour = 60


    require(PartsPerHour % MinutesPerHour == 0)


    val PartsPerMinute = PartsPerHour / MinutesPerHour
}
