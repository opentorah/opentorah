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


object MoonMeanAnomaly {

    val Rambam = Angle(13,3,54)


    val Almagest = Angle(13,3,53,56,17,51,59)


    def main(args: Array[String]) {
        println(Almagest*10)
        println(Almagest*100)
        println(Almagest*1000)
        println(Almagest*10000)
        println(Almagest*29)
        println(Almagest*354)
    }
}
