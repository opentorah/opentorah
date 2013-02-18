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

package org.podval.calendar.astronomical.moon

import org.podval.calendar.astronomical.angle.Angle


object AnomalyMean {

  type Days = Int


  //    val RambamExact = Angle(13,3,53,55,49)
  //    val Almagest = Angle(13,3,53,56,17,51,59)
  val VALUES = List[(Days, Angle)](
    1     -> Angle(13,3,54),
    10    -> Angle(130, 39, 0),
    100   -> Angle(226, 29, 53),
    1000  -> Angle(104, 58, 50),
    10000 -> Angle(329, 48, 20),
    29    -> Angle(18, 53, 4),
    354   -> Angle(305, 0, 13)
  )
}
