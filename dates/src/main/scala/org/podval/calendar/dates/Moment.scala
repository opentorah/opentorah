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


final class Moment private (days: Int, time: Time) extends MomentT[Time, Moment](days, time) {

    def day: Day = Day(days + 1)


    def create(days: Int, hours: Int, parts: Int) = Moment(days, Time(hours, parts))


    def toFullString: String = day.toFullString + " " + time.toFullString
}


object Moment {

    def apply(days: Int, time: Time): Moment = new Moment(days, time)
}
