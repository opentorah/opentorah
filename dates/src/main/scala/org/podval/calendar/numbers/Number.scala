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

  final def head(value: Int): N = set(0, value)

  final def tail(position: Int): Int = get(position+1)

  final def tail(position: Int, value: Int): N = set(position+1, value)

  private final def get(position: Int): Int = {
    val normalDigits: Seq[Int] = normal.digits
    if (normalDigits.length > position) normalDigits(position) else 0
  }

  private final def set(position: Int, value: Int): N = {
    val normalDigits: Seq[Int] = normal.digits
    val newDigits: Seq[Int] = normalDigits.padTo(position+1, 0).updated(position, value)
    fromDigits(newDigits)
  }

  final def length: Int = digits.tail.length

  final def canonical: N = {
    val result: Seq[Int] = numberSystem.withSign(isPositive = true, digits = normal.digits)
    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    val canonicalDigits: Seq[Int] = result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
    fromDigits(canonicalDigits)
  }

  final def simple: N = {
    val thisNormal: N = this.normal
    fromDigits(numberSystem.withSign(!thisNormal.isNegative, thisNormal.digits))
  }

  final def normal: N = fromDigits(numberSystem.normal(this))

  final def signum: Int = nonZeroDigit.map(math.signum).getOrElse(0)

  final def isZero: Boolean = nonZeroDigit.isEmpty

  final def isPositive: Boolean = nonZeroDigit.exists(_ > 0)

  final def isNegative: Boolean = nonZeroDigit.exists(_ < 0)

  private[this] def nonZeroDigit: Option[Int] = normal.digits.find(_ != 0)

  final def abs: N = fromDigits(simple.digits.map(math.abs))

  final def unary_- : N = fromDigits(digits.map(-_))

  final def roundTo(length: Int): N = {
    require(length >= 0)

    def forDigit(digit: Int, position: Int, range: Int): (Int, Int) =
      if (position < length) (0, digit)
      else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0)

    val roundedDigits: Seq[Int] = numberSystem.transform(normal.digits, forDigit, (digit: Int) => digit)

    fromDigits(roundedDigits)
  }

  final def toRational: BigRational = numberSystem.to[BigRational](
    digits,
    (digit: Int, denominator: BigInt) => BigRational(digit, denominator),
    _ + _
  )

  final def toDouble: Double = numberSystem.to[Double](
    digits,
    (digit: Int, denominator: BigInt) => digit.toDouble/denominator.bigInteger.longValueExact(),
    _ + _
  )

  final def toString(length: Int): String = numberSystem.toString(this.simple, length)

  override def toString: String = toString(length)

  final def compare(that: N): Int =
    zipWith(this.simple.digits, that.simple.digits, _ compare _).find (_ != 0).getOrElse(0)


  final override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[N]) false else {
      val that: N = other.asInstanceOf[N]
      (this.numberSystem == that.numberSystem) && (this.companion == that.companion) &&
        (this.compare(that) == 0)
    }
  }

  final override def hashCode: Int =  (73 /: canonical.digits)((v, x) => 41 * v + x)

  protected final def add[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ + _)

  protected final def subtract[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ - _)

  private final def zipWith(
    left: Seq[Int],
    right: Seq[Int],
    operation: (Int, Int) => Int
  ): Seq[Int] =
    left.zipAll(right, 0, 0).map(operation.tupled)
}
