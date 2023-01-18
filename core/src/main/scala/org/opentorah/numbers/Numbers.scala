package org.opentorah.numbers

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
trait Numbers extends NumbersBase:

  open class Digit(val sign: String):
    lazy val position: Int = digitDescriptors.indexOf(this.asInstanceOf[DigitType]) // TODO ordinal?

  type DigitType <: Digit

  private type WithValues[T] = { val values: Array[T] } // TODO move to Collections

  type DigitCompanionType <: WithValues[DigitType]

  def Digit: DigitCompanionType

  // TODO is there any way to pull this in here instead of calling values in every derived number system?
  protected def digitDescriptors: Array[DigitType]

  final override protected lazy val digitSigns: Seq[String] = digitDescriptors.toIndexedSeq.map(_.sign)

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
  abstract class Number[N <: Number[N]](final override val digits: Digits) extends HasDigits with Ordered[N]:
    this: N =>

    // at least the head digit is present
    require(digits.nonEmpty)
    // no trailing 0s
    require(!digits.tail.lastOption.contains(0))

    /** Companion object that was used to create the number. */
    def companion: NumberCompanion[N]

    /** Returns digit described by the [[Digit]] descriptor `digit`. */
    final def get(digit: Digit): Int = get(digit.position)

    /** Returns this number with digit described by the [[Digit]] descriptor `digit` set to `value`. */
    final def set(digit: Digit, value: Int): N = set(digit.position, value)

    /** Returns this number with digit at `position` set to `value`. */
    final def set(position: Int, value: Int): N = companion.fromDigits(setDigits(position, value))

    /** Is this number equal to `0`? */
    final def isZero: Boolean = signum == 0

    /** Is this number greater than `0`? */
    final def isPositive: Boolean = signum > 0

    /** Is this number less than `0`? */
    final def isNegative: Boolean = signum < 0

    /** Returns absolute value of this number. */
    final def abs: N = companion.fromDigits(absDigits)

    /** Returns this number with the sign inverted. */
    @scala.annotation.targetName("uminus")
    final def unary_- : N = companion.fromDigits(negateDigits)

    /** Returns Vector representing difference between `this` and `that` numbers (which must be both Points or both Vectors). */
    @scala.annotation.targetName("subtract")
    final def -(that: N): Vector = Vector.fromDigits(subtractDigits(that))

    /** Returns this number rounded to the digit described by the [[Digit]] descriptor `digit`. */
    final def roundTo(digit: Digit): N = roundTo(digit.position)

    /** Returns this number rounded to the `position`. */
    final def roundTo(length: Int): N = companion.fromDigits(roundToDigits(this, length))

    /** Converts this number to [[org.opentorah.numbers.BigRational]]. */
    final def toRational: BigRational = convertTo[BigRational](this)

    /** Converts this number to `Double`. */
    final def toDouble: Double = convertTo[Double](this)

    /** Returns string representation of this number. */
    override def toString: String = toString(length)
    final def toString(length: Int): String = toStringDigits(this, length)

    /** How does `this` number compare with `that`? */
    final override def compare(that: N): Int = compareDigits(that)

    /** Are the two numbers equal? */
    final override def equals(other: Any): Boolean = this.compare(other.asInstanceOf[N]) == 0

    final override def hashCode: Int = digits.hashCode

  trait NumberCompanion[N <: Number[N]]:
    final lazy val zero: N = apply(0)

    final def apply(digits: Int*): N = fromDigits(digits)

    final def apply(string: String): N = fromDigits(fromStringDigits(string))

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
    final def +(that: Vector): Vector = Vector.fromDigits(addDigits(that))

    /** Returns Point resulting from adding specified Point to this Vector. */
    @scala.annotation.targetName("add")
    final def +(that: Point): Point = Point.fromDigits(addDigits(that))

    /** Returns this Vector multiplied by the specified Int. */
    @scala.annotation.targetName("multiply")
    final def *(n: Int): Vector = Vector.fromDigits(multiplyDigits(n))

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
    final def +(that: Vector): Point = Point.fromDigits(addDigits(that))

    /** Returns Point resulting subtracting specified Vector to this one. */
    @scala.annotation.targetName("subtract")
    final def -(that: Vector): Point = Point.fromDigits(subtractDigits(that))

  open class PointCompanion extends NumberCompanion[Point]:
    final override protected def isCanonical: Boolean = true
    final override protected def newNumber(digits: Seq[Int]): Point = newPoint(digits)

  protected def newPoint(digits: Seq[Int]): Point

object Numbers:

  trait NonPeriodic extends Numbers:
    final override def headRangeOpt: Option[Int] = None

  trait Periodic extends Numbers:
    final override def headRangeOpt: Option[Int] = Some(headRange)

    def headRange: Int
    require(headRange % 2 == 0)

    final lazy val period: Vector = Vector(headRange)
    final lazy val halfPeriod: Vector = Vector(headRange/2)
