package org.opentorah.numbers

trait Numbers {

  type Point <: PointNumber

  val Point: PointCompanion

  type Vector <: VectorNumber

  val Vector: VectorCompanion

  /** Number (Point or Vector) from the number system.
   * Number is a sequence of fractions where numerator is the `digit` and denominator
   * for each position is determined by the number system (the `point` comes after the first digit, which denominator is `1`).
   * Numbers from the same number system and with the same companion can be compared for equality and order.
   *
   * @tparam N  type of the number: `Point` or `Vector`
   *
   * @param digits  sequence of the digits comprising this number.
   */
  // TODO turn N from a type parameter into a type member
  abstract class Number[N <: Number[N]](final val digits: Digits) { this: N =>

    // at least the head digit is present
    require(digits.nonEmpty)
    // no trailing 0s
    require(!digits.tail.lastOption.contains(0))

    /** Companion object that was used to create the number. */
    def companion: NumberCompanion[N]

    /** Returns digit described by the [[Digit]] descriptor `digit`. */
    final def get(digit: Digit): Int = get(digit.position)

    /** Returns digit at `position`. */
    final def get(position: Int): Int = if (digits.length > position) digits(position) else 0

    /** Returns this number with digit described by the [[Digit]] descriptor `digit` set to `value`. */
    final def set(digit: Digit, value: Int): N = set(digit.position, value)

    /** Returns this number with digit at `position` set to `value`. */
    final def set(position: Int, value: Int): N =
      companion.fromDigits(digits.padTo(position+1, 0).updated(position, value))

    /** Returns number of digits after the `point`. */
    final def length: Int = digits.tail.length

    /** Returns the signum of this number: `-1` if it is negative, `1` if it is positive and `0` if it is neither. */
    final def signum: Int = Numbers.this.signum(digits)

    /** Is this number equal to `0`? */
    final def isZero: Boolean = signum == 0

    /** Is this number greater than `0`? */
    final def isPositive: Boolean = signum > 0

    /** Is this number less than `0`? */
    final def isNegative: Boolean = signum < 0

    /** Returns absolute value of this number. */
    final def abs: N = companion.fromDigits(digits.map(math.abs))

    /** Returns this number with the sign inverted. */
    final def unary_- : N = companion.fromDigits(digits.map(-_))

    /** Returns Vector representing difference between `this` and `that` numbers (which must be both Points or both Vectors). */
    final def -(that: N): Vector = {
      require(isComparable(that))
      Vector.fromDigits(subtract(that))
    }

    /** Returns this number rounded to the digit described by the [[Digit]] descriptor `digit`. */
    final def roundTo(digit: Digit): N = roundTo(digit.position)

    /** Returns this number rounded to the `position`. */
    final def roundTo(length: Int): N = companion.fromDigits(Numbers.this.roundTo(digits, length))

    /** Converts this number to [[org.opentorah.numbers.BigRational]]. */
    final def toRational: BigRational = to[BigRational]

    /** Converts this number to `Double`. */
    final def toDouble: Double = to[Double]

    private final def to[T: Convertible]: T = Numbers.this.to[T](digits)

    /** Returns string representation of this number with `length` positions (padding/truncating as needed). */
    final def toString(length: Int): String = Numbers.this.toString(this, length)

    /** Returns string representation of this number. */
    override def toString: String = toString(length)

    /** How does `this` number compare with `that`? */
    final def compare(that: N): Int = {
      require(isComparable(that))
      zipWith(that, _ compare _).find(_ != 0).getOrElse(0)
    }

    /** Are the two numbers equal? */
    final override def equals(other: Any): Boolean = other.isInstanceOf[Number[_]] && {
      val that: N = other.asInstanceOf[N]
      isComparable(that) && (this.compare(that) == 0)
    }

    final override def hashCode: Int = digits.hashCode

    protected final def add(that: Number[_]): Digits = zipWith(that, _ + _)

    // used in PeriodicPoint, so needs to be less than 'protected'
    private[numbers] final def subtract(that: Number[_]): Digits = zipWith(that, _ - _)

    private final def isComparable(that: N): Boolean =
      /* TODO (this.numbers == that.numbers) && */ this.companion == that.companion

    private final def zipWith(
      that: Number[_],
      operation: (Int, Int) => Int
    ): Digits =
      this.digits.zipAll(that.digits, 0, 0).map(operation.tupled)
  }

  trait NumberCompanion[N <: Number[N]] {
    final lazy val zero: N = apply(0)

    final def apply(digits: Int*): N = fromDigits(digits)

    final def fromRational(value: BigRational, length: Int): N = from[BigRational](value, length)

    final def fromDouble(value: Double, length: Int): N = from[Double](value, length)

    private final def from[T: Convertible](value: T, length: Int): N = fromDigits(Numbers.this.from[T](value, length))

    final def fromDigits(digits: Digits): N = newNumber(normalize(digits, isCanonical = isCanonical))

    protected def newNumber(digits: Digits): N

    protected def isCanonical: Boolean
  }

  /** Vector from the number system. */
  abstract class VectorNumber(digits: Digits) extends Number[Vector](digits) { this: Vector =>

    /** Returns Vector resulting from adding specified Vector to this one. */
    final def +(that: Vector): Vector = Vector.fromDigits(add(that))

    /** Returns Point resulting from adding specified Point to this Vector. */
    final def +(that: Point): Point = Point.fromDigits(add(that))

    /** Returns this Vector multiplied by the specified Int. */
    final def *(n: Int): Vector = Vector.fromDigits(digits map (_ * n))

    /** Returns this Vector divided by the specified Int with up to length digits after the point. */
    final def /(n: Int, length: Int): Vector = this.*(BigRational(n).invert, length)

    /** Returns this Vector multiplied by the specified [[org.opentorah.numbers.BigRational]]
      * with up to length digits after the point. */
    final def *(that: BigRational, length: Int): Vector =
      Vector.fromRational(this.toRational*that, math.max(this.length, length))

    /** Returns canonical representation of this Vector;
     * Vectors are not canonicalized by default even in the periodic number systems. */
    final def canonical: Vector = Vector.canonical(digits)
  }

  trait VectorCompanion extends NumberCompanion[Vector] {
    protected final override def isCanonical: Boolean = false

    private[numbers] final def canonical(digits: Digits): Vector =
      Vector.newNumber(normalize(digits, isCanonical = true))
  }

  /** Point from the number system. */
  abstract class PointNumber(digits: Digits) extends Number[Point](digits) { this: Point =>

    /** Returns Point resulting from adding specified Vector to this one. */
    final def +(that: Vector): Point = Point.fromDigits(add(that))

    /** Returns Point resulting subtracting specified Vector to this one. */
    final def -(that: Vector): Point = Point.fromDigits(subtract(that))
  }

  trait PointCompanion extends NumberCompanion[Point] {
    protected final override def isCanonical: Boolean = true
  }

  def headRangeOpt: Option[Int]

  /**
   * Maximum number of digits after the dot.
   *
   * @return maximum number of digits after the dot
   */
  def maxLength: Int

  /**
   *
   * @param position within the tail
   * @return positive, even number
   */
  def range(position: Int): Int

  private[numbers] final lazy val ranges: Seq[Int] = (0 until maxLength).map(range)

  private final lazy val denominators: Seq[BigInt] = {
    def mult(acc: BigInt, tail: Digits): Seq[BigInt] = tail.toList match {
      case Nil => Seq.empty
      case r :: rs => acc +: mult(acc * r, rs)
    }

    mult(BigInt(1), ranges :+ 0)
  }

  val Digit: DigitsDescriptor

  private[numbers] final def to[T: Convertible](digits: Digits): T =
    digits zip denominators.take(digits.length) map (Convertible[T].div _).tupled reduce Convertible[T].plus

  // this can probably be done with digit(i) = value*denominators(i).whole%denominator(i) - but will it be less precise?
  private[numbers] final def from[T: Convertible](value: T, length: Int): Digits = {
    val (digits: Digits, lastReminder /*: T*/) =
      ranges.take(length).foldLeft((Seq.empty[Int], Convertible[T].abs(value))) {
        case ((acc: Digits, reminder /*: T*/), range: Int) =>
          val (whole: Int, fraction /*: T*/) = Convertible[T].wholeAndFraction(reminder)
          (acc :+ whole, Convertible[T].mult(fraction, range))
      }

    (digits :+ Convertible[T].round(lastReminder)).map(Convertible[T].signum(value)*_)
  }

  /** Convert a number to String.
   *
   * If length specified is bugger than the number of digits after the point in the number,
   * missing positions are assumed to have 0s; if the length is smaller, some digits will not
   * be shown.
   * Signs (like 'h' for hours or '°' for degrees) are inserted after digits.
   * If no sign is configured for a position, ',' is used - except for the last digit,
   * where no sign is appended in such a case.
   *
   * @param number to convert to String
   * @param length desired number of digits after the point
   * @tparam N flavor of the number
   * @return String representation of the number
   */
  private[numbers] final def toString[N <: Number[N]](number: N, length: Int): String = {
    val digits: Digits = number.digits.padTo(length+1, 0)
    val signs: Seq[String] = Digit.signs.take(length+1).padTo(length, ",").padTo(length+1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map { case (digit: Int, sign: String) => math.abs(digit).toString + sign }
    (if (number.isNegative) "-" else "") + result.mkString
  }

  private[numbers] final def roundTo(digits: Digits, length: Int): Digits = {
    require(length >= 0)

    transform(
      digits = digits,
      forDigit = (digit: Int, position: Int, range: Int) =>
        if (position < length) (0, digit)
        else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0),
      forHead = identity
    )
  }

  private[numbers] final def normalize(digits: Digits, isCanonical: Boolean): Digits = {
    def t(
      digits: Digits,
      forDigit: (/* digit: */ Int, /* digitRange: */ Int) => (Int, Int)
    ): Digits = transform(
      digits,
      (digit: Int, _ /* TODO position - unused! */: Int, digitRange: Int) => forDigit(digit, digitRange),
      (headDigit: Int) =>
        if (!isCanonical) headDigit
        else headRangeOpt.fold(headDigit){ headRange: Int => forDigit(headDigit, headRange)._2 }
    )

    // fit all digits within their ranges
    val normalDigits: Digits = t(
      digits = if (digits.isEmpty) Seq(0) else digits,
      forDigit = (digit: Int, digitRange: Int) => (digit / digitRange, digit % digitRange)
    )

    // determine the sign of the result
    val willBePositive: Boolean = (signum(normalDigits) >= 0) || (isCanonical && headRangeOpt.isDefined)
    val sign: Int = if (willBePositive) 1 else -1

    // make all digits of the same sign
    val result: Digits = t(
      digits = normalDigits,
      forDigit = (digit: Int, digitRange: Int) =>
        if ((digit == 0) || (math.signum(digit) == sign)) (0, digit) else (-sign, digit + sign * digitRange)
    )

    // drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  private[numbers] final def signum(digits: Digits): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)

  private final def transform(
    digits: Digits,
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Digits = {
    val (headCarry: Int, newTail: Digits) = digits.tail.zipWithIndex.foldRight(0, Seq.empty[Int]) {
      case ((digit: Int, position: Int), (carry: Int, result: Digits)) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }

  // Ordering implicits (ignore bogus Idea warnings)
  // TODO clean up this and the ones in Numbered
  import scala.language.implicitConversions

  implicit val pointOrdering: Ordering[Point] = (x: Point, y: Point) => x.compare(y)

  implicit def pointOrderingOps(lhs: Point): pointOrdering.OrderingOps = pointOrdering.mkOrderingOps(lhs)

  implicit val vectorOrdering: Ordering[Vector] = (x: Vector, y: Vector) => x.compare(y)

  implicit def vectorOrderingOps(lhs: Vector): vectorOrdering.OrderingOps = vectorOrdering.mkOrderingOps(lhs)
}
