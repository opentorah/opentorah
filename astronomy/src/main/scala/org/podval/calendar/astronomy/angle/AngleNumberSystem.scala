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


  protected override val signs: List[String] = List("°", "′", "″", "‴") ++ List.empty.padTo(max_length - 4, ",")


  protected override val ranges: List[Int] = List.empty.padTo(max_length - 1, 60)


  protected override val headRange: Option[Int] = Some(360)

} with NumberSystem {

  protected final override type Interval = Angle


  protected final override val intervalCreator: Creator[Interval] = Angle.apply


  protected final override type Point = AnglePoint


  protected final override val pointCreator: Creator[Point] = AnglePoint.apply



  trait AngleNumber extends Number {

    def degrees: Int = head


    def degrees(value: Int): SelfType = digit(0, value)


    def minutes = digit(1)


    def minutes(value: Int): SelfType = digit(1, value)


    def roundToMinutes: SelfType = roundTo(1)


    def seconds = digit(2)


    def seconds(value: Int): SelfType = digit(2, value)


    def roundToSeconds: SelfType = roundTo(2)


    def thirds  = digit(3)


    def thirds(value: Int): SelfType = digit(3, value)


    def toRadians: Double = math.toRadians(toDegrees)


    def toDegrees: Double = toDouble
  }



  protected final class AnglePoint(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with AngleNumber with PointBase


  protected object AnglePoint {

    def apply(negative: Boolean, digits: List[Int]): AnglePoint = new AnglePoint(negative, digits)
  }


  final class Angle(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with AngleNumber with IntervalBase



  object Angle {

    def apply(digits: Int*): Angle = new Angle(false, digits.toList)


    def apply(negative: Boolean, digits: List[Int]): Angle = new Angle(negative, digits)


    import scala.language.implicitConversions


    implicit def angleToRadians(angle: Angle): Double = angle.toRadians


    def fromRadians(value: Double, length: Int): Angle = fromDegrees(math.toDegrees(value), length)


    def fromDegrees(value: Double, length: Int): Angle = fromDouble(value, length)(intervalCreator)


    def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
      val fullDays = 360.0/approximate.toDegrees
      val fullRotations = math.floor(days/fullDays).toInt
      (360.0*fullRotations+angle.toDegrees)/days
    }
  }
}
