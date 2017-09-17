package org.podval.calendar.numbers

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]] (rawDigits: Seq[Int])
  extends Ordered[N] with NumberSystemMember[S]
{ this: N =>
  val digits: Seq[Int] = if (rawDigits.nonEmpty) rawDigits else Seq(0)

  def companion: NumberCompanion[S, N]

  def toInterval: S#Interval

  final def fromDigits(digits: Seq[Int]): N = companion.fromDigits(digits)

  final def head: Int = numberSystem.get(digits, 0)

  final def head(value: Int): N = fromDigits(numberSystem.set(digits, 0, value))

  final def tail(position: Int): Int = numberSystem.get(digits, position+1)

  final def tail(position: Int, value: Int): N =
    fromDigits(numberSystem.set(digits, position+1, value))

  final def length: Int = digits.tail.length

  final def canonical: N = fromDigits(numberSystem.canonical(digits))

  final def simple: N = fromDigits(numberSystem.simple(digits))

  final def normal: N = fromDigits(numberSystem.normal(digits))

  final def signum: Int = numberSystem.signum(digits)

  final def isZero: Boolean = numberSystem.isZero(digits)

  final def isPositive: Boolean = numberSystem.isPositive(digits)

  final def isNegative: Boolean = numberSystem.isNegative(digits)

  final def abs: N = fromDigits(numberSystem.abs(digits))

  final def unary_- : N = fromDigits(numberSystem.negate(digits))

  final def roundTo(length: Int): N = fromDigits(numberSystem.roundTo(digits, length))

  final def toRational: BigRational = numberSystem.toRational(digits)

  final def toDouble: Double = numberSystem.toDouble(digits)

  final def toString(length: Int): String = numberSystem.toString(digits, length)

  override def toString: String = toString(length)

  final def compare(that: N): Int = numberSystem.compare(this.digits, that.digits)

  final override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Number[S, N]]) false else {
      val that: Number[S, N] = other.asInstanceOf[Number[S, N]]
      (this.numberSystem == that.numberSystem) && (this.companion == that.companion) &&
        (numberSystem.compare(this.digits, that.digits) == 0)
    }
  }

  final override def hashCode: Int = numberSystem.hashCode(digits)
}
