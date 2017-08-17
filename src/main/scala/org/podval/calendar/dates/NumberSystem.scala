package org.podval.calendar.dates

// TODO rename createXXX to mkXXX or newXXX; XXXCreator as createXXX.
trait NumberSystem {

  protected type Creator[T <: Number[T]] = (Boolean, List[Int]) => T

  protected type Point <: PointBase

  protected val pointCreator: Creator[Point]

  protected type Interval <: IntervalBase

  protected val intervalCreator: Creator[Interval]

  protected val signs: List[String]

  protected val ranges: List[Int]

  ranges.foreach { range =>
    require(range > 0)
    require(range % 2 == 0)
  }

  require(signs.length == (ranges.length + 1))

  protected final def maxLength: Int = ranges.length

  private[this] val divisors: List[Double] = ranges.inits.toList.reverse.tail.map(_.product.toDouble)


  trait Number[N <: Number[N]] extends Ordered[N] { this: N =>

    protected val selfCreator: Creator[N]

    def negative: Boolean

    def digits: List[Int]

    final def head: Int = digits.head

    final def tail: List[Int] = digits.tail

    // TODO is this correct - or should it be `digits.length`?
    final def length: Int = tail.length

    final def digit(n: Int): Int = {
      require(0 <= n && n <= maxLength)
      if (length >= n) digits(n) else 0
    }

    final def digit(n: Int, value: Int): N = {
      require(0 <= n && n <= maxLength)
      create(negative, digits.padTo(n + 1, 0).updated(n, value))(selfCreator)
    }

    protected final def plus[T <: Number[T]](that: Number[_])(creator: Creator[T]): T = plusMinus(operationNegation = false, that)(creator)

    protected final def minus[T <: Number[T]](that: Number[_])(creator: Creator[T]): T = plusMinus(operationNegation = true, that)(creator)

    private[this] final def plusMinus[T <: Number[T]](operationNegation: Boolean, that: Number[_])(creator: Creator[T]): T = {
      val sameSign = this.negative == that.negative
      val operationSelector = if (operationNegation) !sameSign else sameSign
      val operation: (Int, Int) => Int = if (operationSelector) _ + _ else _ - _
      create(negative, zip(that).map(operation.tupled))(creator)
    }

    final def toDouble: Double =
      (if (negative) -1 else +1) * (head + ((tail zip divisors) map lift(_ / _)).sum)

    // TODO add rounding tests
    final def roundTo(n: Int): N = {
      require(n >= 0)

      val (more_, toRound) = tail splitAt n
      val tail_ = {
        if (more_.isEmpty) more_
        else {
          val toRoundWithRange = toRound zip ranges.drop(n)
          val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
          more_.init :+ (more_.last + carry)
        }
      }

      create(negative, head +: tail_)(selfCreator)
    }

    private[this] def zip(that: Number[_]): List[(Int, Int)] = this.digits zipAll(that.digits, 0, 0)

    // TODO why can't I inline .tupled?
    private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled

    // TODO: padding; cutting off 0; more flavours...
    protected final def toSignedString: String = {
      val tokens = digits map (_.toString) zip signs flatMap (p => List(p._1, p._2))
      val result = (if (length <= 3) tokens else tokens.init).mkString
      (if (negative) "-" else "") + result
    }

    override def toString: String = toSignedString

    final override def hashCode: Int = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode

    final def compare(that: N): Int = {
      if (this.negative == that.negative) {
        val result = zip(that) map lift(_.compare(_)) find (_ != 0) getOrElse 0
        if (!this.negative) result else -result
      } else {
        if (!that.negative) +1 else -1
      }
    }

    final override def equals(other: Any): Boolean =
    // TODO deal with the "erasure" warning
      if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0

  }


  trait PointBase extends Number[Point] { this: Point =>

    protected override val selfCreator: Creator[Point] = pointCreator

    final def +(that: Interval): Point = plus(that)(pointCreator)

    final def -(that: Interval): Point = minus(that)(pointCreator)

    final def -(that: Point): Interval = minus(that)(intervalCreator)
  }



  trait IntervalBase extends Number[Interval] { this: Interval =>

    protected override val selfCreator: Creator[Interval] = intervalCreator

    final def +(that: Interval): Interval = plus(that)(intervalCreator)

    final def -(that: Interval): Interval = minus(that)(intervalCreator)

    final def *(n: Int): Interval = create(negative, digits map (n * _))(intervalCreator)

    final def /(n: Int): Interval = {
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
      val (newDigits, lastCarry) = (digits.init zip (0 :: ranges.init)).foldLeft(List.empty[Int], 0)(step)
      val lastDigit = lastStep(digits.last, lastCarry, ranges.last)

      create(negative, newDigits :+ lastDigit)(intervalCreator)
    }

    final def %(n: Int): Interval = this - ((this / n) * n)

    final def /(that: Interval): Int = {
      // TODO deal with negativity
      // TODO faster?
      var result = 0
      var done = false
      var subtractee = this

      do {
        subtractee = subtractee - that
        done = subtractee.negative
        if (!done) result += 1
      } while (!done)

      result
    }

    final def %(that: Interval): Interval = this - (that * (this / that))

    // TODO add multiplication (and division, and reminder) on the ScalarNumber from another NumberSystem!
    // How to formulate the type of "Number from some NumberSystem"?

    final def digitsWithRangesForMultiplication: List[(Int, Int)] = digits zip (1 :: ranges)

    final def *(that: NumberSystem#IntervalBase): Interval = {
      val z = create(negative = false, List(0))(intervalCreator)

      def step(elem: (Int, Int), acc: Interval): Interval = {
        val (digit, range) = elem
        (acc + this*digit)/range
      }

      that.digitsWithRangesForMultiplication.foldRight(z)(step)
    }
  }


  abstract class NumberBase[N <: Number[N]](override val negative: Boolean, override val digits: List[Int])
    extends Number[N] { this: N => }


  final def fromDouble[T <: Number[T]](value: Double, length: Int)(creator: Creator[T]): T = {
    val negative = value < 0
    val absValue = if (!negative) value else -value

    val digits = absValue +: ((((1.0d :: divisors.init) zip divisors) take length) map { case (previous, current) =>
      (absValue % (1.0d / previous)) / (1.0d / current)
    })

    create(negative, (digits.init map (math.floor(_).toInt)) :+ math.round(digits.last).toInt)(creator)
  }

  final def create[T <: Number[T]](negative: Boolean, digits: List[Int])(creator: Creator[T]): T = {
    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, range) = elem
      val (carry, result) = acc
      val value = digit + carry
      val (quotient, reminder) = (value / range, value % range)
      val (carry_, digit_) = if (value >= 0) (quotient, reminder) else (quotient - 1, reminder + range)

      (carry_, digit_ :: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val carriedHead = correctHeadDigit(head + headCarry)
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

    checkHeadDigit(newHead)

    (newTail zip ranges) foreach { case (digit, range) =>
      require(digit < range, "must be less than " + range)
    }

    creator(newNegative, newDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int
}
