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
trait Numbers {

  type Number <: NumberBase


  val Number: NumberCompanion


  abstract class NumberBase(val negative: Boolean, val digits: List[Int]) extends Ordered[Number] {

    // Ensure that digits are within appropriate ranges
    digits.foreach(digit => require(digit >= 0, "must be non-negative"))

    if (Number.headRange.isDefined) {
      require(head < Number.headRange.get, "must be less than " + Number.headRange.get)
    }

    (tail zip Number.ranges) foreach { case (digit, range) =>
      require(digit < range, "must be less than " + range)
    }


    final def head: Int = digits.head


    final def tail: List[Int] = digits.tail


    final def length: Int = tail.length


    final def digit(n: Int): Int = if (length >= n) digits(n) else 0


    final def digit(n: Int, value: Int): Number = Number(negative, digits.padTo(n + 1, 0).updated(n, value))


    // TODO check won't work because of the erasure :(
    final override def equals(other: Any): Boolean = if (!other.isInstanceOf[Number]) false
    else {
      val that = other.asInstanceOf[Number]
      (this.negative == that.negative) && (zip(that) forall lift(_ == _))
    }


    final override def hashCode = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode


    final override def compare(that: Number) = {
      if (this.negative == that.negative) {
        val result = zip(that) map lift(_.compare(_)) find (_ != 0) getOrElse (0)
        if (!this.negative) result else -result
      } else {
        if (!this.negative) +1 else -1
      }
    }


    final def +(that: Number): Number = {
      Number(negative, zip(that) map lift(_ + _))
    }


    final def -(that: Number): Number = Number(negative, zip(that) map lift(_ - _))


    final def *(n: Int): Number = Number(negative, digits map (n * _))


    final def /(n: Int): Number = Number(negative, digits map (_ / n)) // TODO make correct :)


    final def %(n: Int): Number = ???


    final def /(that: Number): Int = {
      // TODO deal with negativity
      var result = 0
      var done = false
      var subtractee = this

      do {
        subtractee = subtractee - that
        done = subtractee <= Number(false, List(0))
        if (!done) result += 1
      } while (!done)

      result
    }


    final def %(that: Number): Number = {
      val quotient = (this / that)
      this - that * quotient
    }


    final def toDouble: Double = (if (negative) -1 else +1) * (head + ((tail zip Number.divisors) map lift(_ / _)).sum)


    private[this] def zip(that: NumberBase) = digits zipAll(that.digits, 0, 0)


    // TODO why can't I inline .tupled?
    private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled


    // TODO: padding; cutting off 0; more flavours...
    final override def toString: String = {
      val tokens = digits map (_.toString) zip Number.signs flatMap (p => List(p._1, p._2))
      val result = (if (length <= 3) tokens else tokens.init).mkString
      (if (negative) "-" else "") + result
    }
  }


  abstract class NumberCompanion {

    val headRange: Option[Int]


    val ranges: List[Int]


    ranges.foreach { range =>
      require(range > 0)
      require(range % 2 == 0)
    }


    final val divisors: List[Double] = ranges.inits.toList.reverse.tail.map { upto: List[Int] => upto.fold(1)(_ * _).toDouble}


    val signs: List[String]


    final def apply(head: Int, tail: Int*): Number = apply(head :: tail.toList)


    final def apply(digits: List[Int]): Number = {
      // TODO collapse into the next one with sign=+1
      val negative = digits.head < 0
      val absDigits = if (!negative) digits else ((-digits.head) :: digits.tail)
      apply(negative, absDigits)
    }


    final def apply(negative: Boolean, digits: List[Int]): Number = {
      def step(elem: (Int, Int), acc: (Int, List[Int])) = {
        val (digit, range) = elem
        val (carry, result) = acc
        val v = digit + carry
        val (quotient, reminder) = (v / range, v % range)
        val (carry_, digit_) = if (v >= 0) (quotient, reminder) else (quotient - 1, reminder + range)
        (carry_, digit_ :: result)
      }

      val (headCarry, carriedTail) = ((digits.tail zip ranges) :\(0, List.empty[Int]))(step)

      val v = digits.head + headCarry

      val carriedHead = if (headRange.isEmpty) v
      else {
        val result = v % headRange.get
        if (v >= 0) result else result + headRange.get // TODO maybe the overall "negative" needs to be corrected?
      }

      create(negative, carriedHead :: carriedTail)
    }


    protected def create(negative: Boolean, digits: List[Int]): Number


    final def fromDouble(value: Double, length: Int): Number = {
      val negative = value < 0
      val absValue = if (!negative) value else -value

      val digits = absValue +: ((((1.0d :: divisors.init) zip divisors) take length) map { case (previous, current) =>
        (absValue % (1.0d / previous)) / (1.0d / current)
      })

      apply(negative, (digits.init map (math.floor(_).toInt)).toList :+ math.round(digits.last).toInt)
    }


    final def roundTo(what: Number, n: Int): Number = {
      require(n >= 0)

      val (more_, toRound) = what.tail splitAt n
      val tail_ = {
        if (more_.isEmpty) more_
        else {
          val toRoundWithRange = toRound zip divisors.drop(n)
          val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
          more_.init :+ (more_.last + carry)
        }
      }

      apply(what.negative, what.head +: tail_)
    }
  }
}
