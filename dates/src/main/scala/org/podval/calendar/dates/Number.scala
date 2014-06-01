package org.podval.calendar.dates

/*
 * Copyright 2014 Podval Group.
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
abstract class Number(val negative: Boolean, val digits: List[Int]) extends Ordered[Number] {

  val Number: NumberCompanion


  // Ensure that digits are within appropriate ranges
  digits.foreach(digit => require(digit >= 0, "must be non-negative"))

  if (headRange.isDefined) {
    require(head < headRange.get, "must be less than " + headRange.get)
  }

  (tail zip ranges) foreach { case (digit, range) =>
    require(digit < range, "must be less than " + range)
  }


  final def head: Int = digits.head


  final def tail: List[Int] = digits.tail


  final def length: Int = tail.length


  final def digit(n: Int): Int = if (length >= n) digits(n) else 0


  private[this] final def headRange: Option[Int] = Number.headRange


  private[this] final def ranges: List[Int] = Number.ranges


  // TODO check won't work because of the erasure :(
  final override def equals(other: Any): Boolean = if (!other.isInstanceOf[Number.T]) false else {
    val that = other.asInstanceOf[Number.T]
    (this.negative == that.negative) && (zip(that) forall lift(_==_))
  }


  final override def hashCode = (73 /: digits) ((v,x) => 41*v+x) + negative.hashCode


  final override def compare(that: Number) = {
    if (this.negative == that.negative) {
      val result = zip(that) map lift(_.compare(_)) find(_!= 0) getOrElse(0)
      if (!this.negative) result else - result
    } else {
      if (!this.negative) +1 else -1
    }
  }


  def +(that: Number.T): Number.T = Number(negative, zip(that) map lift(_+_))


  def -(that: Number.T): Number.T = Number(negative, zip(that) map lift(_-_))


  def *(n: Int): Number.T = Number(negative, digits map (n*_))


  private[this] def zip(that: Number) = digits zipAll (that.digits, 0, 0)


  // TODO use f.tupled instead!!!
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = p => op(p._1, p._2)


  // TODO: padding
  override def toString: String = {
    val tokens = digits map (_.toString) zip Number.signs flatMap (p => List(p._1, p._2))
    val result = (if (length <= 3) tokens else tokens.init).mkString
    (if (negative) "-" else "") + result
  }
}



abstract class NumberCompanion {

  type T <: Number


  def headRange: Option[Int]


  def ranges: List[Int]


  def signs: List[String]


  final def apply(head: Int, tail: Int*): T = apply(head :: tail.toList)


  final def apply(digits: List[Int]): T = {
    val negative = digits.head < 0
    val absDigits = if (!negative) digits else ((- digits.head) :: digits.tail)
    apply(negative, absDigits)
  }


  final def apply(negative: Boolean, digits: List[Int]): T = {
    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, range) = elem
      val (carry, result) = acc
      val v = digit + carry
      val (quotient, reminder) = (v / range, v % range)
      val (carry_, digit_) = if (v >= 0) (quotient, reminder) else (quotient-1, reminder+range)
      (carry_, digit_ :: result)
    }

    val (headCarry, carriedTail) = ((digits.tail zip ranges) :\ (0, List.empty[Int]))(step)

    val v = digits.head + headCarry

    val carriedHead = if (headRange.isEmpty) v else {
      val result = v % headRange.get
      if (v >= 0) result else result + headRange.get // TODO maybe the overall "negative" needs to be corrected?
    }

    create(negative, carriedHead :: carriedTail)
  }


  protected def create(negative: Boolean, digits: List[Int]): T


  def roundTo(what: T, n: Int): T = {
    require(n >= 0)

    val (more_, toRound) = what.tail splitAt n
    val more__ = {
      if (more_.isEmpty) more_ else {
        // TODO use real ranges!!!
        val carry = (toRound :\ 0)((x, c) => if (x + c >= 30) 1 else 0)
        more_.init :+ (more_.last + carry)
      }
    }

    apply(what.negative, what.head +: more__)
  }
}
