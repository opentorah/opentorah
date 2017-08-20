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

package org.podval.calendar.astronomy.sun

import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.DayData


object LongitudeMean extends DayData {

  override val value = Map[Days, Angle](
    1     -> Angle(  0, 59,  8),
    10    -> Angle(  9, 51, 23),
    100   -> Angle( 98, 33, 53),
    1000  -> Angle(265, 38, 50), // remainder
    10000 -> Angle(136, 28, 20),
    29    -> Angle( 28, 35,  1), // reconstructs if calculated as 3*v(10)-v(1); not if using the exact value :)
    354   -> Angle(348, 55, 15)  // ??
  )


  val exact_ = Angle(0, 59, 8, 19, 48)  // TODO why was it 49?

  override val almagestValue = Angle(0, 59, 8, 17, 13, 12, 31)



  def main(args: Array[String]) {
    def m(n: Int) = (exact_ * n).roundTo(2)
    for (n <- List(1, 10, 100, 1000, 10000, 354))
      println(n + " " + m(n))

    val v29 = Angle(9, 51, 23)*3 - Angle(0, 59, 8)
    println(v29)

    val v354 = Angle(98, 33, 53)*3 + Angle(9, 51, 23)*5 + Angle(0, 59, 8)*4
    println(v354)
  }
}
