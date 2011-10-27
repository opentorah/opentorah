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


object MoonMeanLongitude extends MultiplicationTable(Map(
    1     -> Angle(13,10,35),
    10    -> Angle(131, 45, 50),
    100   -> Angle(237, 38, 23),
    1000  -> Angle(216, 23, 50),
    10000 -> Angle(3, 58, 20),
    29    -> Angle(22, 6, 56),
    354   -> Angle(344, 26, 43)
)) {

//    val exact = Angle(13,10,35,1,48,1)
//    val exactInDegrees = 13.176397222222223


//    val Almagest = Angle(13,10,34,58,33,30,30)


    def main(args: Array[String]) {
        
        println("exact= " + exactInDegrees + ":" + exact)
//        print()
    }
}
