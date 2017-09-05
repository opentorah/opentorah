package org.podval.calendar.numbers

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]] (digits: Seq[Int])
  extends Ordered[N] with NumberSystemMember[S]
{ this: N =>
  require(digits.nonEmpty)
  zipWithRanges.foreach { case (digit, range) =>
    require(digit >= 0, s"$digit must be non-negative")
    require(digit < range, s"$digit must be less than $range")
  }
  if (tail_.nonEmpty) require(tail_.last != 0)

  protected def fromDigits(digits: Seq[Int]): N

  final def head: Int = digits.head

  final def head(value: Int): N = if (head == value) this else fromDigits(value +: tail_)

  final def absHead: Int = math.abs(head)

  final def signum: Int = if (isZero) 0 else if (isNegative) -1 else +1

  final def isZero: Boolean = (head == 0) && tail_.isEmpty

  final def isPositive: Boolean = !isZero && !isNegative

  final def isNegative: Boolean = head < 0

  final def abs: N = head(absHead)

  final def unary_- : N = head(-head)

  private final def tail_ : Seq[Int] = digits.tail

  final def length: Int = tail_.length

  final def tail(position: Int): Int = if (length > position) tail_(position) else 0

  final def tail(position: Int, value: Int): N =
    if (tail(position) == value) this
    else fromDigits(head +: tail_.padTo(position+1, 0).updated(position, value))

  protected final def add(negate: Boolean, that: Number[S, _]): Seq[Int] = {
    val invertOperation: Boolean =
      negate ^ (!this.isZero && !that.isZero && (this.isNegative != that.isNegative))
    val newSignum: Int = if (this.isZero) that.signum else this.signum
    val result: Seq[Int] = zipWith(that, if (!invertOperation) _ + _ else _ - _)
    newSignum*result.head +: result.tail
  }

  // TODO handle negativity
  final def roundTo(length: Int): N = {
    require(length >= 0)

    val (toRetain, toRound) = tail_ splitAt length
    val toRoundWithRange = toRound.zipWithIndex.map {
      case (digit, position) => (digit, numberSystem.range(length+position))
    }
    val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}

    fromDigits(
      if (toRetain.isEmpty) Seq(head + carry)
      else  head +: toRetain.init :+ (toRetain.last + carry)
    )
  }

  final def toRational: BigRational = {
    def forDigit(digit: Int, denominator: BigInt): BigRational =
      BigRational(digit, denominator)
    to[BigRational](forDigit, _ + _) * signum
  }

  final def toDouble: Double = {
    def forDigit(digit: Int, denominator: BigInt): Double =
      digit.toDouble/denominator.bigInteger.longValueExact()
    to[Double](forDigit, _ + _) * signum
  }

  private[this] final def to[T](forDigit: (Int, BigInt) => T, plus: (T, T) => T): T = {
    val zeroDenominator: BigInt = BigInt(1)
    zipWithRanges.foldLeft((forDigit(absHead, zeroDenominator), zeroDenominator)) {
      case ((acc: T, denominator: BigInt), (digit: Int, range: Int)) =>
        val newDenominator: BigInt = denominator*range
        (plus(acc, forDigit(digit, newDenominator)), newDenominator)
    }._1
  }

  protected final def zipWithRanges: Seq[(Int, Int)] =
    tail_.zipWithIndex.map { case (digit, position) => (digit, numberSystem.range(position)) }

  private[this] def zipWith(that: Number[S, _], operation: (Int, Int) => Int): Seq[Int] =
    (this.absHead +: this.tail_).zipAll(that.absHead +: that.tail_, 0, 0).map(operation.tupled)

  final def toString(length: Int): String = {
    def digitToString(defaultSign: String)(pair: (Int, Option[String])): String = {
      val (digit: Int, sign: Option[String]) = pair
      digit + sign.getOrElse(defaultSign)
    }

    val digitsWithSigns: Seq[(Int, Option[String])] = tail_.padTo(length, 0).zipWithIndex.map {
      case (digit, position) => (digit, numberSystem.sign(position))
    }
    val tailResult: Seq[String] =
      if (digitsWithSigns.isEmpty) Seq.empty
      else digitsWithSigns.init.map(digitToString(",")) :+ digitToString("")(digitsWithSigns.last)

    val result: Seq[String] = (head + numberSystem.headSign) +: tailResult

    result.mkString
  }

  override def toString: String = toString(length)

  // This needs to be overridden for the PeriodicNumber, so it isn't final.
  override def hashCode: Int = digitsHashCode

  final def digitsHashCode: Int = (73 /: digits)((v, x) => 41 * v + x)

  // This needs to be overridden for the PeriodicNumber, so it isn't final.
  def compare(that: N): Int = (if (this.signum != that.signum) 1 else compareDigits(that)) * signum

  final def compareDigits(that: N): Int = zipWith(that, _ compare _).find (_ != 0) getOrElse 0

  final override def equals(other: Any): Boolean =
    // TODO deal with the "erasure" warning; compare numberSystem...
    if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0
}
