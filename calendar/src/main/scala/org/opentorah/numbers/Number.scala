package org.opentorah.numbers

/** Number (Point or Vector) from the number system `S`.
  * Number is a sequence of fractions where numerator is the `digit` and denominator
  * for each position is determined by the number system (the `point` comes after the first digit, which denominator is `1`).
  * Numbers from the same number system and with the same companion can be compared for equality and order.
  *
  * @tparam S  type of the number system
  * @tparam N  type of the number: `S#Point` or `S#Vector`
  *
  * @param digits  sequence of the digits comprising this number.
  */
// TODO turn N from a type parameter into a type member;
// deal with the `Ordered[N]`
abstract class Number[S <: Numbers[S], N <: Number[S, N]](numbers: S, final val digits: Digits)
  extends NumbersMember[S](numbers) with Ordered[N]
{ this: N =>

  // at least the head digit is present
  require(digits.nonEmpty)
  // no trailing 0s
  require(!digits.tail.lastOption.contains(0))

  /** Companion object that was used to create the number. */
  def companion: NumberCompanion[S, N]

  /** Returns digit described by the [[Digit]] descriptor `digit`. */
  final def get(digit: Digit): Int = get(digit.position)

  /** Returns digit at `position`. */
  final def get(position: Int): Int = if (digits.length > position) digits(position) else 0

  /** Returns this number with digit described by the [[Digit]] descriptor `digit` set to `value`. */
  final def set(digit: Digit, value: Int): N = set(digit.position, value)

  /** Returns this number with digit at `position` set to `value`. */
  final def set(position: Int, value: Int): N =
    fromDigits(digits.padTo(position+1, 0).updated(position, value))

  /** Returns number of digits after the `point`. */
  final def length: Int = digits.tail.length

  /** Returns the signum of this number: `-1` if it is negative, `1` if it is positive and `0` if it is neither. */
  final def signum: Int = numbers.signum(digits)

  /** Is this number equal to `0`? */
  final def isZero: Boolean = signum == 0

  /** Is this number greater than `0`? */
  final def isPositive: Boolean = signum > 0

  /** Is this number less than `0`? */
  final def isNegative: Boolean = signum < 0

  /** Returns absolute value of this number. */
  final def abs: N = fromDigits(digits.map(math.abs))

  /** Returns this number with the sign inverted. */
  final def unary_- : N = fromDigits(digits.map(-_))

  /** Returns Vector representing difference between `this` and `that` numbers (which must be both Points or both Vectors). */
  final def -(that: N): S#Vector = {
    require(isComparable(that))
    numbers.Vector.fromDigits(subtract(that))
  }

  /** Returns this number rounded to the digit described by the [[Digit]] descriptor `digit`. */
  final def roundTo(digit: Digit): N = roundTo(digit.position)

  /** Returns this number rounded to the `position`. */
  final def roundTo(length: Int): N = fromDigits(numbers.roundTo(digits, length))

  /** Converts this number to [[BigRational]]. */
  final def toRational: BigRational = to[BigRational]

  /** Converts this number to `Double`. */
  final def toDouble: Double = to[Double]

  private final def to[T: Convertible]: T = numbers.to[T](digits)

  /** Returns string representation of this number with `length` positions (padding/truncating as needed). */
  final def toString(length: Int): String = numbers.toString(this, length)

  /** Returns string representation of this number. */
  override def toString: String = toString(length)

  /** How does `this` number compare with `that`? */
  final override def compare(that: N): Int = {
    require(isComparable(that))
    zipWith(that, _ compare _).find(_ != 0).getOrElse(0)
  }

  /** Are the two numbers equal? */
  final override def equals(other: Any): Boolean = other.isInstanceOf[Number[_, _]] && {
    val that: N = other.asInstanceOf[N]
    isComparable(that) && (this.compare(that) == 0)
  }

  final override def hashCode: Int = digits.hashCode

  protected final def fromDigits(digits: Digits): N = companion.fromDigits(digits)

  protected final def add[N1 <: Number[S, N1]](that: N1): Digits = zipWith(that, _ + _)

  // used in PeriodicPoint, so neds to be less than 'protected'
  private[numbers] final def subtract[N1 <: Number[S, N1]](that: N1): Digits = zipWith(that, _ - _)

  private final def isComparable(that: N): Boolean =
    (this.numbers == that.numbers) && (this.companion == that.companion)

  private final def zipWith[N1 <: Number[S, N1]](
    that: N1,
    operation: (Int, Int) => Int
  ): Digits =
    this.digits.zipAll(that.digits, 0, 0).map(operation.tupled)
}
