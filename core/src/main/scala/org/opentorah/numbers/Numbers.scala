package org.opentorah.numbers

type Digits = Seq[Int]

/** Number System.
 *
 * Each number system `S` (derived from [[org.opentorah.numbers.Numbers]])
 * supports two flavors of [[org.opentorah.numbers.Numbers#Number]]:
 * `S#Point` (derived from [[org.opentorah.numbers.Numbers#PointNumber]] and
 * `S#Vector` (derived from [[org.opentorah.numbers.Numbers#VectorNumber]].
 *
 * This distinction allows definitions of operations to be constrained to the types where
 * a give operation makes sense:
 * `-: (Vector, Vector) => Vector` with right unit `Vector.zero`;
 * `+: (Vector, Vector) => Vector` with unit `Vector.zero`;
 * `-: (Point, Point) => Vector`;
 * `+: (Point, Vector) => Point` with left unit `Point.zero` and its "commutation"
 * `+: (Vector, Point) => Point` with right unit `Point.zero`.
 *
 * This distinction may seem to be artificial:
 * after all, given operations `-: (Point, Point) => Vector` and `+: (Point, Vector) => Point`,
 * we have an isomorphism between `Points` and `Vectors`: `ptov(p) = p - Point.zero`, `vtop(v) = Point.zero + v`.
 *
 * Indeed, there is not much difference between the two when we are talking about angles.
 * However, for dates (`Point`s) it makes sense to ask what year/month the date is in -
 * but not for time intervals (`Vector`s)!
 */
trait Numbers:

  open class Digit(val sign: String):
    lazy val position: Int = digitDescriptors.indexOf(this.asInstanceOf[DigitType]) // TODO ordinal?

  type DigitType <: Digit

  type WithValues[T] = { val values: Array[T] } // TODO move to Collections

  type DigitCompanionType <: WithValues[DigitType]

  def Digit: DigitCompanionType

  // TODO is there any way to pull this in here instead of calling values in every derived number system?
  protected def digitDescriptors: Array[DigitType]

  type Point <: PointNumber

  given CanEqual[Point, Point] = CanEqual.derived

  type PointCompanionType <: PointCompanion

  final val Point: PointCompanionType = createPointCompanion

  protected def createPointCompanion: PointCompanionType

  type Vector <: VectorNumber

  given CanEqual[Vector, Vector] = CanEqual.derived

  type VectorCompanionType <: VectorCompanion

  final val Vector: VectorCompanionType = createVectorCompanion

  protected def createVectorCompanion: VectorCompanionType

  /** Number (Point or Vector) from the number system.
   * Number is a sequence of fractions where numerator is the `digit` and denominator
   * for each position is determined by the number system (the `point` comes after the first digit, which denominator is `1`).
   * Numbers from the same number system and with the same companion can be compared for equality and order.
   *
   * @tparam N  type of the number: `Point` or `Vector`
   *
   * @param digits  sequence of the digits comprising this number.
   */
  // TODO extend scala.math.Numeric
  abstract class Number[N <: Number[N]](final val digits: Digits) extends Ordered[N]:
    this: N =>

    // at least the head digit is present
    require(digits.nonEmpty)
    // no trailing 0s
    require(!digits.tail.lastOption.contains(0))

    /** Companion object that was used to create the number. */
    def companion: NumberCompanion[N]

    /** Returns digit described by the [[Digit]] descriptor `digit`. */
    final def get(digit: Digit): Int = get(digit.position)

    /** Returns digit at `position`. */
    final def get(position: Int): Int = if digits.length > position then digits(position) else 0

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
    @scala.annotation.targetName("uminus")
    final def unary_- : N = companion.fromDigits(digits.map(-_))

    /** Returns Vector representing difference between `this` and `that` numbers (which must be both Points or both Vectors). */
    @scala.annotation.targetName("subtract")
    final def -(that: N): Vector = Vector.fromDigits(subtract(that))

    /** Returns this number rounded to the digit described by the [[Digit]] descriptor `digit`. */
    final def roundTo(digit: Digit): N = roundTo(digit.position)

    /** Returns this number rounded to the `position`. */
    final def roundTo(length: Int): N =
      require(length >= 0)

      val result: Digits = transform(
        digits = digits,
        forDigit = (digit: Int, position: Int, range: Int) =>
          if position < length then (0, digit)
          else (if math.abs(digit) >= range / 2 then math.signum(digit) else 0, 0),
        forHead = identity
      )
      companion.fromDigits(result)

    /** Converts this number to [[org.opentorah.numbers.BigRational]]. */
    final def toRational: BigRational = convertTo[BigRational](digits)

    /** Converts this number to `Double`. */
    final def toDouble: Double = convertTo[Double](digits)

    // TODO fromString()?
    
    /** Convert a number to String.
     *
     * If length specified is bugger than the number of digits after the point in the number,
     * missing positions are assumed to have 0s; if the length is smaller, some digits will not
     * be shown.
     * Signs (like 'h' for hours or '°' for degrees) are inserted after digits.
     * If no sign is configured for a position, ',' is used - except for the last digit,
     * where no sign is appended in such a case.
     *
     * @param length desired number of digits after the point
     * @return String representation of the number
     */
    final def toString(length: Int): String =
      val digits: Digits = this.digits.padTo(length+1, 0)
      val signs: Seq[String] = digitSigns.take(length+1).padTo(length, ",").padTo(length+1, "")

      // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
      val result: Seq[String] = (digits zip signs) map((digit: Int, sign: String) => math.abs(digit).toString + sign)
      (if isNegative then "-" else "") + result.mkString

    /** Returns string representation of this number. */
    override def toString: String = toString(length)

    /** How does `this` number compare with `that`? */
    final override def compare(that: N): Int = zipWith(that.digits, _ compare _).find(_ != 0).getOrElse(0)

    /** Are the two numbers equal? */
    final override def equals(other: Any): Boolean = this.compare(other.asInstanceOf[N]) == 0

    final override def hashCode: Int = digits.hashCode

    protected final def add(that: Number[?]): Digits = add(that.digits)

    protected final def add(thatDigits: Digits): Digits = zipWith(thatDigits, _ + _)

    protected final def subtract(that: Number[?]): Digits = zipWith(that.digits, _ - _)

    private def zipWith(
      thatDigits: Digits,
      operation: (Int, Int) => Int
    ): Digits =
      this.digits.zipAll(thatDigits, 0, 0).map(operation.tupled)

  trait NumberCompanion[N <: Number[N]]:
    final lazy val zero: N = apply(0)

    final def apply(digits: Int*): N = fromDigits(digits)

    final def fromRational(value: BigRational, length: Int): N = fromDigits(convertFrom[BigRational](value, length))

    final def fromDouble(value: Double, length: Int): N = fromDigits(convertFrom[Double](value, length))

    final def fromDigits(digits: Digits, isCanonical: Boolean = isCanonical): N =
      newNumber(getDigits(digits, isCanonical))

    protected def newNumber(digits: Digits): N

    protected def isCanonical: Boolean

    final class Interval(val from: N, val to: N):
      require(from <= to)

      override def toString: String = s"[$from..$to]"

      def contains(value: N): Boolean = (from <= value) && (value <= to)

      def intersect(that: Interval): Interval = Interval(
        from = if this.from < that.from then that.from else this.from,
        to =   if this.to   > that.to   then that.to   else this.to
      )

  /** Vector from the number system. */
  abstract class VectorNumber(digits: Digits) extends Number[Vector](digits):
    this: Vector =>

    final override def companion: VectorCompanionType = Vector

    /** Returns Vector resulting from adding specified Vector to this one. */
    @scala.annotation.targetName("add")
    final def +(that: Vector): Vector = Vector.fromDigits(add(that))

    /** Returns Point resulting from adding specified Point to this Vector. */
    @scala.annotation.targetName("add")
    final def +(that: Point): Point = Point.fromDigits(add(that))

    /** Returns this Vector multiplied by the specified Int. */
    @scala.annotation.targetName("multiply")
    final def *(n: Int): Vector = Vector.fromDigits(digits map (_ * n))

    /** Returns this Vector divided by the specified Int with up to length digits after the point. */
    @scala.annotation.targetName("divide")
    final def /(n: Int, length: Int): Vector = this.*(BigRational(n).invert, length)

    /** Returns this Vector multiplied by the specified [[org.opentorah.numbers.BigRational]]
      * with up to length digits after the point. */
    @scala.annotation.targetName("multiply")
    final def *(that: BigRational, length: Int): Vector =
      Vector.fromRational(this.toRational*that, math.max(this.length, length))

    /** Returns canonical representation of this Vector;
     * Vectors are not canonicalized by default even in the periodic number systems. */
    final def canonical: Vector = Vector.fromDigits(digits, isCanonical = true)

  open class VectorCompanion extends NumberCompanion[Vector]:
    final override protected def isCanonical: Boolean = false
    final override protected def newNumber(digits: Seq[Int]): Vector = newVector(digits)

  protected def newVector(digits: Seq[Int]): Vector

  /** Point from the number system. */
  abstract class PointNumber(digits: Digits) extends Number[Point](digits):
    this: Point =>

    final override def companion: PointCompanionType = Point

    /** Returns Point resulting from adding specified Vector to this one. */
    @scala.annotation.targetName("add")
    final def +(that: Vector): Point = Point.fromDigits(add(that))

    /** Returns Point resulting subtracting specified Vector to this one. */
    @scala.annotation.targetName("subtract")
    final def -(that: Vector): Point = Point.fromDigits(subtract(that))

  open class PointCompanion extends NumberCompanion[Point]:
    final override protected def isCanonical: Boolean = true
    final override protected def newNumber(digits: Seq[Int]): Point = newPoint(digits)

  protected def newPoint(digits: Seq[Int]): Point

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

  private final lazy val denominators: Seq[BigInt] =
    def mult(acc: BigInt, tail: Digits): Seq[BigInt] = tail.toList match
      case Nil => Seq.empty
      case r :: rs => acc +: mult(acc * r, rs)

    mult(BigInt(1), ranges :+ 0)

  private lazy val digitSigns: Seq[String] = digitDescriptors.toIndexedSeq.map(_.sign)

  private def signum(digits: Digits): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)

  private def transform(
    digits: Digits,
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Digits =
    val (headCarry: Int, newTail: Digits) = digits.tail.zipWithIndex.foldRight(0, Seq.empty[Int]) {
      case ((digit: Int, position: Int), (carry: Int, result: Digits)) =>
        val (resultCarry: Int, resultDigit: Int) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail

  final protected def convertTo[T: Convertible](digits: Digits)(using convertible: Convertible[T]): T =
    digits.zip(denominators.take(digits.length))
      .map(convertible.div.tupled)
      .reduce(convertible.plus)

    // this can probably be done with digit(i) = value*denominators(i).whole%denominator(i) - but will it be less precise?
  private def convertFrom[T: Convertible](value: T, length: Int)(using convertible: Convertible[T]): Digits =
    val (digits: Digits, lastReminder: T) = ranges.take(length)
      .foldLeft((Seq.empty[Int], convertible.abs(value))) { case ((acc: Digits, reminder: T), range: Int) =>
        val (whole: Int, fraction: T) = convertible.wholeAndFraction(reminder)
        (acc :+ whole, convertible.mult(fraction, range))
      }

    (digits :+ convertible.round(lastReminder)).map(convertible.signum(value)*_)

  final protected def getDigits(digits: Digits, isCanonical: Boolean = false): Digits =
    def t(
      digits: Digits,
      forDigit: (/* digit: */ Int, /* digitRange: */ Int) => (Int, Int)
    ): Digits = transform(
      digits,
      (digit: Int, _ /* TODO position - unused! */: Int, digitRange: Int) => forDigit(digit, digitRange),
      (headDigit: Int) =>
        if !isCanonical then headDigit
        else headRangeOpt.fold(headDigit)((headRange: Int) => forDigit(headDigit, headRange)._2)
    )

    // fit all digits within their ranges
    val normalDigits: Digits = t(
      digits = if digits.isEmpty then Seq(0) else digits,
      forDigit = (digit: Int, digitRange: Int) => (digit / digitRange, digit % digitRange)
    )

    // determine the sign of the result
    val willBePositive: Boolean = (signum(normalDigits) >= 0) || (isCanonical && headRangeOpt.isDefined)
    val sign: Int = if willBePositive then 1 else -1

    // make all digits of the same sign
    val preResult: Digits = t(
      digits = normalDigits,
      forDigit = (digit: Int, digitRange: Int) =>
        if (digit == 0) || (math.signum(digit) == sign) then (0, digit) else (-sign, digit + sign * digitRange)
    )

    // drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    preResult.head +: preResult.tail.reverse.dropWhile(_ == 0).reverse

object Numbers:

  trait NonPeriodic extends Numbers:
    final override def headRangeOpt: Option[Int] = None

  trait Periodic extends Numbers:
    final override def headRangeOpt: Option[Int] = Some(headRange)

    def headRange: Int

    require(headRange % 2 == 0)

    final lazy val period: Vector = Vector(headRange)

    final lazy val halfPeriod: Vector = Vector(headRange/2)
