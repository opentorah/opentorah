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
trait NumberSystem {

  type Creator[T <: Number] = (Boolean, List[Int]) => T



  trait Number {

    protected type SelfType <: Number


    protected def selfCreator: Creator[SelfType]


    def negative: Boolean


    def digits: List[Int]


    final def head: Int = digits.head


    final def tail: List[Int] = digits.tail


    final def length: Int = tail.length


    final def digit(n: Int): Int = if (length >= n) digits(n) else 0


    final def digit(n: Int, value: Int): SelfType = {
      // TODO check that n is not more than max length
      create(negative, digits.padTo(n + 1, 0).updated(n, value))(selfCreator)
    }


    final override def hashCode = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode


    protected final def compare_(that: SelfType) = {
      if (this.negative == that.negative) {
        val result = zip(that) map lift(_.compare(_)) find (_ != 0) getOrElse (0)
        if (!this.negative) result else -result
      } else {
        if (!that.negative) +1 else -1
      }
    }


    protected final def equals_(that: SelfType): Boolean = compare_(that) == 0


    final def plus[T <: Number](that: Number)(creator: Creator[T]): T = {
      val operation: (Int, Int) => Int = if (this.negative == that.negative) (_ + _) else (_ - _)
      create(negative, zip(that).map(lift(operation)))(creator)
    }


    final def minus[T <: Number](that: Number)(creator: Creator[T]): T =
      create(negative, zip(that).map(lift (if (this.negative == that.negative) (_ - _) else (_ + _))))(creator)


    private[this] def zip(that: Number): List[(Int, Int)] = this.digits zipAll(that.digits, 0, 0)


    // TODO why can't I inline .tupled?
    private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled


    final def toDouble(value: Number): Double =
      (if (value.negative) -1 else +1) * (value.head + ((value.tail zip divisors) map lift(_ / _)).sum)


    final def roundTo(n: Int): SelfType = {
      require(n >= 0)

      val (more_, toRound) = tail splitAt n
      val tail_ = {
        if (more_.isEmpty) more_
        else {
          val toRoundWithRange = toRound zip divisors.drop(n)
          val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
          more_.init :+ (more_.last + carry)
        }
      }

      create(negative, head +: tail_)(selfCreator)
    }


    // TODO: padding; cutting off 0; more flavours...
    protected final def toSignedString: String = {
      val tokens = digits map (_.toString) zip signs flatMap (p => List(p._1, p._2))
      val result = (if (length <= 3) tokens else tokens.init).mkString   // TODO chop off 0s!
      (if (negative) "-" else "") + result
    }


    override def toString: String = toSignedString
  }



  trait ScalarNumber extends Number {

    protected type SelfType <: ScalarNumber


    final def +(that: SelfType): SelfType = plus(that)(selfCreator)


    final def -(that: SelfType): SelfType = minus(that)(selfCreator)


    final def *(n: Int): SelfType = create(negative, digits map (n * _))(selfCreator)


    final def /(n: Int): SelfType = {

      def step(acc: (List[Int], Int), elem: (Int, Int)) = {
        val (digit, range) = elem
        val (result, carry) = acc
        val value = digit + carry*range
        val (quotient, reminder) = (value / n, value % n)

        (result :+ quotient, reminder)
      }

      def lastStep(last: Int, lastCarry: Int, lastRange: Int): Int = {
        val value = last + lastCarry*lastRange
        val (quotient, reminder) = (value / n, value % n)

        val roundUp = ((n % 2 == 0) && (reminder >= n / 2)) || ((n % 2 == 1) && (reminder > n / 2))

        if (roundUp) quotient+1 else quotient
      }


      val digits = this.digits.padTo(maxLength+1, 0)
      val (newDigits, lastCarry) = ((digits.init zip (0 :: ranges.init)).foldLeft(List.empty[Int], 0))(step)
      val lastDigit = lastStep(digits.last, lastCarry, ranges.last)

      create(negative, newDigits :+ lastDigit)(selfCreator)
    }


    final def %(n: Int): SelfType = this.minus((this / n) * n)(selfCreator)


    final def /(that: SelfType): Int = {
      // TODO deal with negativity
      // TODO faster?
      var result = 0
      var done = false
      var subtractee = this

      do {
        subtractee = subtractee.minus(that)(selfCreator)
        done = subtractee.negative
        if (!done) result += 1
      } while (!done)

      result
    }


    final def %(that: SelfType): SelfType = this.minus(that * (this / that))(selfCreator)
  }



  abstract class NumberBase(override val negative: Boolean, override val digits: List[Int]) extends Number



  protected val signs: List[String]


  protected val headRange: Option[Int]


  protected val ranges: List[Int]


  ranges.foreach { range =>
    require(range > 0)
    require(range % 2 == 0)
  }

  require(signs.length == (ranges.length + 1))


  final def maxLength = ranges.length


  private[this] val divisors: List[Double] = ranges.inits.toList.reverse.tail.map { upto: List[Int] => upto.fold(1)(_ * _).toDouble}


  final def fromDouble[T <: Number](value: Double, length: Int)(creator: Creator[T]): T = {
    val negative = value < 0
    val absValue = if (!negative) value else -value

    val digits = absValue +: ((((1.0d :: divisors.init) zip divisors) take length) map { case (previous, current) =>
      (absValue % (1.0d / previous)) / (1.0d / current)
    })

    create(negative, (digits.init map (math.floor(_).toInt)).toList :+ math.round(digits.last).toInt)(creator)
  }


  final def create[T <: Number](negative: Boolean, digits: List[Int])(creator: Creator[T]): T = {

    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, range) = elem
      val (carry, result) = acc
      val value = digit + carry
      val (quotient, reminder) = (value / range, value % range)
      val (carry_, digit_) = if (value >= 0) (quotient, reminder) else (quotient - 1, reminder + range)

      (carry_, digit_ :: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val value = head + headCarry

      val carriedHead = if (headRange.isEmpty) value else {
        val result = value % headRange.get
        if (value >= 0) result else result + headRange.get
      }

      val carriedNegative = carriedHead < 0
      val newHead = if (!carriedNegative) carriedHead else -carriedHead

      (carriedNegative, newHead)
    }


    val (headCarry, newTail) = ((digits.tail zip ranges) :\(0, List.empty[Int]))(step)
    val (carriedNegative, newHead) = headStep(digits.head, headCarry)

    val newNegative = if (negative) !carriedNegative else carriedNegative
    val newDigits = newHead :: newTail

    // Ensure that digits are within appropriate ranges
    newDigits.foreach(digit => require(digit >= 0, "must be non-negative"))

    if (headRange.isDefined) {
      require(newHead < headRange.get, "must be less than " + headRange.get)
    }

    (newTail zip ranges) foreach { case (digit, range) =>
      require(digit < range, "must be less than " + range)
    }

    creator(newNegative, newDigits)
  }
}
