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

import org.podval.calendar.dates.NumberSystem


object AngleNumberSystem extends {

  private val max_length = 10


  override val signs: List[String] = List("°", "′", "″", "‴") ++ List.empty.padTo(max_length - 4, ",")


  override val ranges: List[Int] = List.empty.padTo(max_length - 1, 60)


  protected override val headRange: Option[Int] = Some(360)

} with NumberSystem {


  final class Angle(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with ScalarNumber with Ordered[Angle] {

    protected override type SelfType = Angle


    protected override def selfCreator = Angle.creator


    final override def compare(that: Angle): Int = compare_(that)


    final override def equals(other: Any): Boolean =
      if (!other.isInstanceOf[Angle]) false else equals_(other.asInstanceOf[Angle])


    def degrees: Int = head


    def degrees(value: Int): Angle = digit(0, value)


    def minutes = digit(1)


    def minutes(value: Int): Angle = digit(1, value)


    def roundToMinutes: Angle = roundTo(1)


    def seconds = digit(2)


    def seconds(value: Int): Angle = digit(2, value)


    def roundToSeconds: Angle = roundTo(2)


    def thirds  = digit(3)


    def thirds(value: Int): Angle = digit(3, value)


    def toRadians: Double = math.toRadians(toDegrees)


    def toDegrees: Double = toDouble
  }



  object Angle extends {

    def apply(digits: Int*): Angle = new Angle(false, digits.toList)


    def apply(negative: Boolean, digits: List[Int]): Angle = new Angle(negative, digits)


    val creator: Creator[Angle] = apply _


    import scala.language.implicitConversions


    implicit def angleToRadians(angle: Angle): Double = angle.toRadians


    def fromRadians(value: Double, length: Int): Angle = fromDegrees(math.toDegrees(value), length)


    def fromDegrees(value: Double, length: Int): Angle = fromDouble(value, length)(creator)


    def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
      val fullDays = 360.0/approximate.toDegrees
      val fullRotations = math.floor(days/fullDays).toInt
      (360.0*fullRotations+angle.toDegrees)/days
    }
  }
}
