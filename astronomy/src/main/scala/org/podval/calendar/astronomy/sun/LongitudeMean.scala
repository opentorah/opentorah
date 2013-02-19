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

package org.podval.calendar.astronomy.sun

import org.podval.calendar.astronomy.angle.Angle
import org.podval.calendar.astronomy.DayData


object LongitudeMean extends DayData {

  override val value = Map[Days, Angle](
    1     -> Angle(  0, 59,  8),
    10    -> Angle(  9, 51, 23),
    100   -> Angle( 98, 33, 53),
    1000  -> Angle(265, 38, 50), // remainder
    10000 -> Angle(136, 28, 20),
    29    -> Angle( 28, 35,  1),
    354   -> Angle(348, 55, 15)
  )


  val exact_ = Angle(0, 59, 8, 19, 49)

  override val almagestValue = Angle(0, 59, 8, 17, 13, 12, 31)
}
