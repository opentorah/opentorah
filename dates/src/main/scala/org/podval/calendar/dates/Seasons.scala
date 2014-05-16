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

package org.podval.calendar.dates

import Jewish._


object Seasons {
  // TODO Where and when was the Sun created? Does this jibe with Rambam's epoch?
  // TODO Which day of the week (+1/-1) was the Giving of the Law? (Sema)
  // TODO Rambam's epoch - two days after molad?! (Petya Ofman)

  // KH 9:3

  // TODO add convenience methods to clean this up

  val FirstTkufasNissan = Year(1).month(Month.Nisan).newMoon - Moment(0, 7, 9, 642)


  val YearOfShmuel = days(365) + hours(6)


  val YearOfRavAda = Month.MeanLunarPeriod * Year.monthsInCycle / 19


  // Since Birkas HaChama is said in the morning, we add 12 hours to the time of the equinox
  def birkasHachama(cycle: Int) = FirstTkufasNissan + YearOfShmuel * 28 * cycle + hours(12)


  def tkufasNissan(year: Int) = FirstTkufasNissan + YearOfRavAda * (year-1)
}
