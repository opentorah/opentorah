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

package org.podval.calendar.astronomy

import angle.AngleNumberSystem.Angle


trait DayData {

  type Days = Int


  val value: Map[Days, Angle]


  final def keys = value.keys.toList.sorted


  final def rambamValue = value(1)


  final def calculated(days: Days) = rambamValue*days


  final def exact = reconstructed(10000)


  final def exact10 = reconstructed10(10000)


  final def reconstructed(days: Days) = Angle.fromDegrees(reconstructed10(days), 6) // 6 60-digits


  final def reconstructed10(days: Days) = Angle.exactify(rambamValue, days, value(days))


  final def recalculated(days: Days) = exact*days


  final def recalculated10(days: Days) = Angle.fromDegrees(exact10*days, 6)


  val almagestValue: Angle


  final def almagest(days: Days) = almagestValue*days
}
