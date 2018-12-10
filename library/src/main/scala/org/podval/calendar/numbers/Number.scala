package org.podval.calendar.numbers

trait Number[S <: Numbers[S], N <: Number[S, N]] extends Ordered[N] with NumbersMember[S] { this: N =>
  def digits: Seq[Int]

  def companion: NumberCompanion[S, N]

  def toVector: S#Vector

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
    val result: Seq[Int] = companion.withSign(isPositive = true, digits = normal.digits)
    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    val canonicalDigits: Seq[Int] = result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
    fromDigits(canonicalDigits)
  }

  final def simple: N = {
    val thisNormal: N = this.normal
    fromDigits(companion.withSign(!thisNormal.isNegative, thisNormal.digits))
  }

  final def normal: N = fromDigits(numbers.transform(digits, companion.normalDigit, companion.normalHead))

  final def signum: Int = normal.digits.find(_ != 0).map(math.signum).getOrElse(0)

  final def isZero: Boolean = signum == 0

  final def isPositive: Boolean = signum > 0

  final def isNegative: Boolean = signum < 0

  final def abs: N = fromDigits(simple.digits.map(math.abs))

  final def unary_- : N = fromDigits(digits.map(-_))

  final def roundTo(length: Int): N = {
    require(length >= 0)

    def forDigit(digit: Int, position: Int, range: Int): (Int, Int) =
      if (position < length) (0, digit)
      else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0)

    val roundedDigits: Seq[Int] = numbers.transform(normal.digits, forDigit, (digit: Int) => digit)

    fromDigits(roundedDigits)
  }

  final def to[T: Convertible]: T = numbers.to[T](digits)
  final def toRational: BigRational = to[BigRational]
  final def toDouble: Double = to[Double]

  final def toString(length: Int): String = numbers.toString(this.simple, length)

  override def toString: String = toString(length)

  final override def compare(that: N): Int =
    zipWith(this.simple.digits, that.simple.digits, _ compare _).find (_ != 0).getOrElse(0)


  final override def equals(other: Any): Boolean = other.isInstanceOf[Number[_, _]] && {
    val that: N = other.asInstanceOf[N]
    (this.numbers == that.numbers) && (this.companion == that.companion) && (this.compare(that) == 0)
  }

  final override def hashCode: Int = canonical.digits.hashCode

  protected final def add[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ + _)

  protected final def subtract[N1 <: Number[S, N1]](that: N1): Seq[Int] = zipWith(this.digits, that.digits, _ - _)

  private final def zipWith(
    left: Seq[Int],
    right: Seq[Int],
    operation: (Int, Int) => Int
  ): Seq[Int] =
    left.zipAll(right, 0, 0).map(operation.tupled)
}
