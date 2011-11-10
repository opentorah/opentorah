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

package org.podval.calendar.moon


object DayTablesData {

    type Days = Int

    val Order = List[Days](1, 10, 100, 1000, 10000, 29, 354)


//    val exact = Angle(13,10,35,1,48,1)
//    val exactInDegrees = 13.176397222222223
//    val Almagest = Angle(13,10,34,58,33,30,30)
    val moonMeanLongitude = List[(Days, Angle)](
        1     -> Angle(13,10,35),
        10    -> Angle(131, 45, 50),
        100   -> Angle(237, 38, 23),
        1000  -> Angle(216, 23, 50),
        10000 -> Angle(3, 58, 20),
        29    -> Angle(22, 6, 56),
        354   -> Angle(344, 26, 43)
    )


//    val RambamExact = Angle(13,3,53,55,49)
//    val Almagest = Angle(13,3,53,56,17,51,59)
    val moonMeanAnomaly = List[(Days, Angle)](
        1     -> Angle(13,3,54),
        10    -> Angle(130, 39, 0),
        100   -> Angle(226, 29, 53),
        1000  -> Angle(104, 58, 50),
        10000 -> Angle(329, 48, 20),
        29    -> Angle(18, 53, 4),
        354   -> Angle(305, 0, 13)
    )
}
