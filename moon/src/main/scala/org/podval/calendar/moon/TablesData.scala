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


object TablesData {

    type Days = Int

    val SunMeanLongitude = List[(Days, Angle)](
        1     -> Angle(  0, 59,  8),
        10    -> Angle(  9, 51, 23),
        100   -> Angle( 98, 33, 53), 
        1000  -> Angle(265, 38, 50), // remainder
        10000 -> Angle(136, 28, 20),
        29    -> Angle( 28, 35,  1),
        354   -> Angle(348, 55, 15)
    )


    val SunApogee = List[(Days, Angle)](
        10    -> Angle(0,  0,  1, 30),
        100   -> Angle(0,  0, 15),
        1000  -> Angle(0,  2, 30),
        10000 -> Angle(0, 25),
        29    -> Angle(0,  0,  4), // TODO: veod!
        354   -> Angle(0,  0, 53)
    )


//    val exact = Angle(13,10,35,1,48,1)
//    val exactInDegrees = 13.176397222222223
//    val Almagest = Angle(13,10,34,58,33,30,30)
    val MoonMeanLongitude = List[(Days, Angle)](
        1     -> Angle(13,10,35),
        10    -> Angle(131, 45, 50),
        100   -> Angle(237, 38, 23),
        1000  -> Angle(216, 23, 50),
        10000 -> Angle(3, 58, 20),
        29    -> Angle(22, 6, 56),
        354   -> Angle(344, 26, 43)
    )


    val MoonLongitudeCorrection = List[(String, Angle, Angle)](
        ("middle of Taurus"        , Angle( 15), Angle(0)),
        ("beginning of Gemini"     , Angle( 30), Angle(0, 15)),
        ("beginning of Leo"        , Angle( 90), Angle(0, 15)),
        ("middle of Virgo"         , Angle(135), Angle(0, 15)),
        ("middle of Libra"         , Angle(165), Angle(0)),
        ("beginning of Sagittarius", Angle(210), Angle(0, -15)),
        ("beginning of Aquarius"   , Angle(270), Angle(0, -30)),
        ("middle of Pisces"        , Angle(315), Angle(0, -15))
    )


//    val RambamExact = Angle(13,3,53,55,49)
//    val Almagest = Angle(13,3,53,56,17,51,59)
    val MoonMeanAnomaly = List[(Days, Angle)](
        1     -> Angle(13,3,54),
        10    -> Angle(130, 39, 0),
        100   -> Angle(226, 29, 53),
        1000  -> Angle(104, 58, 50),
        10000 -> Angle(329, 48, 20),
        29    -> Angle(18, 53, 4),
        354   -> Angle(305, 0, 13)
    )
}
