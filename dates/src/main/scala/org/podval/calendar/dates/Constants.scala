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


object Constants {

  val DaysPerWeek = 7


  val HoursPerDay = 24


  require(HoursPerDay % 2 == 0)


  val HoursPerHalfDay = HoursPerDay / 2


  val PartsPerHour = 1080


  val MinutesPerHour = 60


  require(PartsPerHour % MinutesPerHour == 0)


  val PartsPerMinute = PartsPerHour / MinutesPerHour


  // It seems that first day of the first year was Sunday.
  val FirstDayDayOfWeekJewish = 1


  val FirstDayDayOfWeekGregorian = 1 // XXX calculate


  val GregorianEpoch = 1373429
}
