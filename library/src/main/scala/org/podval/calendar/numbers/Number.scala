package org.podval.calendar.numbers

import DigitsDescriptor.Digit

trait Number[S <: Numbers[S], N <: Number[S, N]] extends Ordered[N] with NumbersMember[S] { this: N =>
  def digits: Seq[Int]

  def companion: NumberCompanion[S, N]

  protected final def fromDigits(digits: Seq[Int]): N = companion.fromDigits(digits)

  final def get(digit: Digit): Int = get(digit.position)

  final def get(position: Int): Int = if (digits.length > position) digits(position) else 0

  final def set(digit: Digit, value: Int): N = set(digit.position, value)

  final def set(position: Int, value: Int): N = {
    val newDigits: Seq[Int] = digits.padTo(position+1, 0).updated(position, value)
    fromDigits(newDigits)
  }

  final def length: Int = digits.tail.length

  final def signum: Int = numbers.signum(digits)

  final def isZero: Boolean = signum == 0

  final def isPositive: Boolean = signum > 0

  final def isNegative: Boolean = signum < 0

  final def abs: N = fromDigits(numbers.normalize(digits, isCanonical = false).map(math.abs))

  final def unary_- : N = fromDigits(digits.map(-_))

  final def -(that: N): S#Vector = numbers.Vector.fromDigits(subtract(that))

  final def roundTo(digit: Digit): N = roundTo(digit.position)

  final def roundTo(length: Int): N = fromDigits(numbers.roundTo(digits, length))

  final def toRational: BigRational = to[BigRational]
  final def toDouble: Double = to[Double]
  final def to[T: Convertible]: T = numbers.to[T](digits)

  final def toString(length: Int): String = numbers.toString(this, length)

  override def toString: String = toString(length)

  final override def compare(that: N): Int =
    zipWith(this.digits, that.digits, _ compare _).find (_ != 0).getOrElse(0)

  final override def equals(other: Any): Boolean = other.isInstanceOf[Number[_, _]] && {
    val that: N = other.asInstanceOf[N]
    (this.numbers == that.numbers) && (this.companion == that.companion) && (this.compare(that) == 0)
  }

  final override def hashCode: Int = digits.hashCode

  protected final def add[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ + _)

  protected final def subtract[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ - _)

  private final def zipWith(
    left: Seq[Int],
    right: Seq[Int],
    operation: (Int, Int) => Int
  ): Seq[Int] =
    left.zipAll(right, 0, 0).map(operation.tupled)
}
