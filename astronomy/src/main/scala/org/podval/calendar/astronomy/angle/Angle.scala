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


final class Angle(val negative: Boolean, val digits: List[Int]) extends Ordered[Angle] {
  require(!digits.isEmpty)

  (digits zip Angle.RANGES) foreach { case (digit, range) =>
    require(digit >= 0, "must be non-negative")
    require(digit < range, "must be less than " + range)
  }

  def degrees = digits.head
  def more = digits.tail
  def length = digits.length-1
  def minutes = if (length >= 1) digits(1) else 0
  def seconds = if (length >= 2) digits(2) else 0
  def thirds  = if (length >= 3) digits(3) else 0


  override def equals(other: Any): Boolean = other match {
    // TODO mod 360!
    case that: Angle => (this.negative == that.negative) && (zip(that) forall lift(_==_))
    case _ => false
  }


  override def hashCode = (73 /: digits) ((v,x) => 41*v+x) + negative.hashCode


  override def compare(that: Angle) = {
    def comp = zip(that) map lift(_.compare(_)) find(_!= 0) getOrElse(0)

    if (!this.negative && !that.negative) comp else
    if (this.negative && that.negative) - comp else
    if (!this.negative && that.negative) +1 else -1
  }


  def +(other: Angle): Angle = Angle(negative, zip(other) map lift(_+_))


  def -(other: Angle): Angle = Angle(negative, zip(other) map lift(_-_))


  def *(n: Int): Angle = Angle(negative, digits map (n*_))


  private[this] def zip(that: Angle) = digits zipAll (that.digits, 0, 0)


  // TODO use f.tupled instead!!!
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = p => op(p._1, p._2)


  // TODO: padding
  override def toString: String = {
    val tokens = digits map (_.toString) zip Angle.SIGNS flatMap (p => List(p._1, p._2))
    val result = (if (length <= 3) tokens else tokens.init).mkString
    (if (negative) "-" else "") + result
  }


  def toRadians: Double = math.toRadians(toDegrees)


  def toDegrees: Double = (if (negative) -1 else +1) * (degrees + ((more zip Angle.QUOTIENTS) map lift(_/_)).sum)
}



object Angle {

  import scala.language.implicitConversions


  implicit def angleToRadians(angle: Angle): Double = angle.toRadians


  // In Haskell, this'd be a lazy infinite list; in Scala, it has to be finite.
  // I thought that we do not need more than Almagest is using, but it seems that we might...
  val MAX_LENGTH = 100
  val QUOTIENTS = ((1 to MAX_LENGTH) map (n => math.pow(60.0, n)))
  val RANGES = 360 :: List().padTo(MAX_LENGTH-1, 60)


  val SIGNS = List("°", "′", "″", "‴") ++ List().padTo(MAX_LENGTH-3, ",")


  def apply(degrees: Int, more: Int*): Angle = Angle(degrees :: more.toList)


  def apply(digits: List[Int]): Angle = {
    val negative = digits.head < 0
    val absDigits = if (!negative) digits else ((- digits.head) :: digits.tail)
    Angle(negative, absDigits)
  }


  def apply(negative: Boolean, digits: List[Int]): Angle = {
    def step(a: (Int, Int), s: (Int, List[Int])) = {
      val (digit, range) = a
      val (carry, result) = s
      val v = digit + carry
      val (quotient, reminder) = (v / range, v % range)
      val (carry_, digit_) = if (v >= 0) (quotient, reminder) else (quotient-1, reminder+range)
      (carry_, digit_ :: result)
    }

    val carriedDigits = ((digits zip RANGES) :\ (0, List[Int]()))(step)._2

    new Angle(negative, carriedDigits)
  }


  def fromRadians(value: Double, length: Int): Angle = fromDegrees(math.toDegrees(value), length)


  def fromDegrees(value: Double, length: Int): Angle = {
    val negative = value < 0
    val absValue = if (!negative) value else - value

    val digits = absValue +: ((QUOTIENTS take length) map (q => (absValue % (60.0/q))/(1.0/q)))
    Angle(negative, (digits.init map (math.floor(_).toInt)).toList :+ math.round(digits.last).toInt)
  }


  def roundToSeconds(angle: Angle): Angle = roundTo(angle, 2)


  def roundToMinutes(angle: Angle): Angle = roundTo(angle, 1)


  def roundTo(angle: Angle, n: Int): Angle = {
    require(n >= 0)

    val (more_, toRound) = angle.more splitAt n
    val more__ = {
      if (more_.isEmpty) more_ else {
        val carry = (toRound :\ 0)((x, c) => if (x + c >= 30) 1 else 0)
        more_.init :+ (more_.last + carry)
      }
    }

    Angle(angle.negative, angle.degrees +: more__)
  }


  def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullDays = 360.0/approximate.toDegrees
    val fullRotations = math.floor(days/fullDays).toInt
    (360.0*fullRotations+angle.toDegrees)/days
  }
}
