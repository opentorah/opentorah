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
package org.podval.calendar.astronomy.angle

import org.podval.calendar.dates.{NumberCompanion, Number}


final class Angle(negative: Boolean, digits: List[Int]) extends { override val Number = Angle} with Number(negative, digits) {

  def degrees: Int = head
  def minutes = digit(1)
  def seconds = digit(2)
  def thirds  = digit(3)


  def toRadians: Double = math.toRadians(toDegrees)


  def toDegrees: Double = toDouble
}



object Angle extends NumberCompanion {

  import scala.language.implicitConversions


  implicit def angleToRadians(angle: Angle): Double = angle.toRadians


  override type T = Angle


  private val MAX_LENGTH = 100


  override val headRange: Option[Int] = Some(360)


  override val ranges: List[Int] = List.empty.padTo(MAX_LENGTH-1, 60)


  override val quotients: List[Double] = ((1 to MAX_LENGTH) map (n => math.pow(60.0, n))).toList


  override val signs: List[String] = List("°", "′", "″", "‴") ++ List.empty.padTo(MAX_LENGTH-3, ",")


  protected override def create(negative: Boolean, digits: List[Int]): Angle = new Angle(negative, digits)


  def fromRadians(value: Double, length: Int): Angle = fromDegrees(math.toDegrees(value), length)


  def fromDegrees(value: Double) = fromDouble(value)


  def roundToSeconds(angle: Angle): Angle = roundTo(angle, 2)


  def roundToMinutes(angle: Angle): Angle = roundTo(angle, 1)


  def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullDays = 360.0/approximate.toDegrees
    val fullRotations = math.floor(days/fullDays).toInt
    (360.0*fullRotations+angle.toDegrees)/days
  }
}
