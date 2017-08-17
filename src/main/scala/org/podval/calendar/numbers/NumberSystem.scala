package org.podval.calendar.numbers

trait NumberSystem {
  import NumberSystem.RawNumber

  protected type Point <: PointBase

  final def newPoint(raw: RawNumber): Point = createPoint(normalize(raw))

  protected def createPoint(raw: RawNumber): Point

  protected type Interval <: IntervalBase

  final def newInterval(raw: RawNumber): Interval = createInterval(normalize(raw))

  protected def createInterval(raw: RawNumber): Interval

  protected val signs: List[String]

  protected val ranges: List[Int]

  ranges.foreach { range =>
    require(range > 0)
    require(range % 2 == 0)
  }

  require(signs.length == (ranges.length + 1))

  val maxLength: Int = ranges.length

  val divisors: List[Double] = ranges.inits.toList.reverse.tail.map(_.product.toDouble)

  private final def normalize(raw: RawNumber): RawNumber = {
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

    val (negative, digits) = raw
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

    (newNegative, newDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  final def fromDouble(value: Double, length: Int): RawNumber = {
    val negative = value < 0
    val absValue = if (!negative) value else -value

    val digits = absValue +: ((((1.0d :: divisors.init) zip divisors) take length) map { case (previous, current) =>
      (absValue % (1.0d / previous)) / (1.0d / current)
    })

    normalize(negative, (digits.init map (math.floor(_).toInt)) :+ math.round(digits.last).toInt)
  }


  abstract class Number[N <: Number[N]](raw: RawNumber) extends Ordered[N] { this: N =>
    protected final def newPoint(raw: RawNumber): Point = NumberSystem.this.newPoint(raw)
    protected final def newInterval(raw: RawNumber): Interval = NumberSystem.this.newInterval(raw)

    protected def newN(raw: RawNumber): N

    final def negative: Boolean = raw._1

    final def digits: List[Int] = raw._2

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
      newN(negative, digits.padTo(n + 1, 0).updated(n, value))
    }

    protected final def plus(that: Number[_]): RawNumber = plusMinus(operationNegation = false, that)

    protected final def minus(that: Number[_]): RawNumber = plusMinus(operationNegation = true, that)

    private[this] final def plusMinus(operationNegation: Boolean, that: Number[_]): RawNumber = {
      val sameSign = this.negative == that.negative
      val operationSelector = if (operationNegation) !sameSign else sameSign
      val operation: (Int, Int) => Int = if (operationSelector) _ + _ else _ - _
      (negative, zip(that).map(operation.tupled))
    }

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

      newN(negative, head +: tail_)
    }

    final def toDouble: Double =
      (if (negative) -1 else +1) * (head + ((tail zip divisors) map lift(_ / _)).sum)

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


  class PointBase(raw: RawNumber) extends Number[Point](raw) { this: Point =>
    protected final override def newN(raw: RawNumber): Point = newPoint(raw)

    final def +(that: Interval): Point = newPoint(plus(that))

    final def -(that: Interval): Point = newPoint(minus(that))

    final def -(that: Point): Interval = newInterval(minus(that))
  }


  class IntervalBase(raw: RawNumber) extends Number[Interval](raw) { this: Interval =>
    protected final override def newN(raw: RawNumber): Interval =  newInterval(raw)

    final def +(that: Interval): Interval = newInterval(plus(that))

    final def -(that: Interval): Interval = newInterval(minus(that))

    final def *(n: Int): Interval = newInterval(negative, digits map (n * _))

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

      newInterval(negative, newDigits :+ lastDigit)
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
      val z = newInterval(false, List(0))

      def step(elem: (Int, Int), acc: Interval): Interval = {
        val (digit, range) = elem
        (acc + this*digit)/range
      }

      that.digitsWithRangesForMultiplication.foldRight(z)(step)
    }
  }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])
}
