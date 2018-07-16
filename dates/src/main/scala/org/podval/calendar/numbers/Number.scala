package org.podval.calendar.numbers

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]] (rawDigits: Seq[Int])
  extends Ordered[N] with NumberSystemMember[S]
{ this: N =>
  val digits: Seq[Int] = if (rawDigits.nonEmpty) rawDigits else Seq(0)

  def companion: NumberCompanion[S, N]

  def toInterval: S#Interval

  def toPoint: S#Point

  protected final def fromDigits(digits: Seq[Int]): N = companion.fromDigits(digits)

  final def head: Int = get(0)

  final def head(value: Int): N = fromDigits(set(0, value))

  final def tail(position: Int): Int = get(position+1)

  final def tail(position: Int, value: Int): N = fromDigits(set(position+1, value))

  private final def get(position: Int): Int = {
    val normalDigits: Seq[Int] = numberSystem.normal(this)
    if (normalDigits.length > position) normalDigits(position) else 0
  }

  private final def set(position: Int, value: Int): Seq[Int] = {
    val normalDigits: Seq[Int] = numberSystem.normal(this)
    normalDigits.padTo(position+1, 0).updated(position, value)
  }

  final def length: Int = digits.tail.length

  final def canonical: N = fromDigits(numberSystem.canonical(this))

  final def simple: N = fromDigits(numberSystem.simple(this))

  final def normal: N = fromDigits(numberSystem.normal(this))

  final def signum: Int = numberSystem.signum(this)

  final def isZero: Boolean = numberSystem.isZero(this)

  final def isPositive: Boolean = numberSystem.isPositive(this)

  final def isNegative: Boolean = numberSystem.isNegative(this)

  final def abs: N = fromDigits(numberSystem.simple(this).map(math.abs))

  final def unary_- : N = fromDigits(numberSystem.negate(digits))

  final def roundTo(length: Int): N = fromDigits(numberSystem.roundTo(this, length))

  final def toRational: BigRational = numberSystem.toRational(this)

  final def toDouble: Double = numberSystem.toDouble(this)

  final def toString(length: Int): String = numberSystem.toString(this, length)

  override def toString: String = toString(length)

  final def compare(that: N): Int = numberSystem.compare(this, that)

  final override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[N]) false else {
      val that: N = other.asInstanceOf[N]
      (this.numberSystem == that.numberSystem) && (this.companion == that.companion) &&
        (this.compare(that) == 0)
    }
  }

  final override def hashCode: Int =  (73 /: numberSystem.canonical(this))((v, x) => 41 * v + x)
}
