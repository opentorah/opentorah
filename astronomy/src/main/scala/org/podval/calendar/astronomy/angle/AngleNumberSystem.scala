package org.podval.calendar.astronomy.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitNumberSystem


class AngleNumberSystem extends {

  private val max_length = 10


  protected override val signs: List[String] = List("°", "′", "″", "‴") ++ List.empty.padTo(max_length - 4, ",")


  protected override val ranges: List[Int] = List.empty.padTo(max_length - 1, 60)


  final override val headRange: Int = 360

} with RangedHeadDigitNumberSystem[AngleNumberSystem] {

  protected final override type Interval = Angle


  final override def createPoint(raw: RawNumber): Angle = new Angle(raw)


  protected final override type Point = AnglePoint


  final override def createPoint(raw: RawNumber): AnglePoint = new AnglePoint(raw)



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



  final class AnglePoint(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with AngleNumber with PointBase



  object AnglePoint {

    def apply(negative: Boolean, digits: List[Int]): AnglePoint = createPoint(negative, digits)
  }



  final class Angle(negative: Boolean, digits: List[Int]) extends NumberBase(negative, digits) with AngleNumber with IntervalBase



  object Angle {

    def apply(digits: Int*): Angle = new Angle(false, digits.toList)


    def apply(negative: Boolean, digits: List[Int]): Angle = createInterval(raw)


    import scala.language.implicitConversions


    implicit def angleToRadians(angle: Angle): Double = angle.toRadians


    def fromRadians(value: Double, length: Int): Angle = fromDegrees(math.toDegrees(value), length)


    def fromDegrees(value: Double, length: Int): Angle = fromDouble(value, length)


    def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
      val fullDays = 360.0/approximate.toDegrees
      val fullRotations = math.floor(days/fullDays).toInt
      (360.0*fullRotations+angle.toDegrees)/days
    }
  }
}


object AngleNumberSystem extends AngleNumberSystem
