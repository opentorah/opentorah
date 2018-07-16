package org.podval.calendar.numbers

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  def createPoint(digits: Seq[Int]): Point

  val Point: PointCompanion[S]

  type Interval <: IntervalBase[S]

  def createInterval(digits: Seq[Int]): Interval

  val Interval: IntervalCompanion[S]

  val defaultLength: Int

  /**
    *
    * @param position within the tail
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSign: String

  val signPartial: PartialFunction[Int, String]

  final def sign(position: Int): Option[String] = signPartial.lift(position)

  final def signum[N <: Number[S, N]](number: N): Int = nonZeroDigit(number.digits).map(math.signum).getOrElse(0)

  final def isZero[N <: Number[S, N]](number: N): Boolean = nonZeroDigit(number.digits).isEmpty

  final def isPositive[N <: Number[S, N]](number: N): Boolean = nonZeroDigit(number.digits).exists(_ > 0)

  final def isNegative[N <: Number[S, N]](number: N): Boolean = isNegative(number.digits)
  final def isNegative(digits: Seq[Int]): Boolean = nonZeroDigit(digits).exists(_ < 0)

  private[this] def nonZeroDigit(digits: Seq[Int]): Option[Int] = normal(digits).find(_ != 0)

  final def compare[N <: Number[S, N]](left: N, right: N): Int =
    zipWith(simple(left), simple(right), _ compare _).find (_ != 0) getOrElse 0

  final def negate(digits: Seq[Int]): Seq[Int] = digits.map(-_)

  final def add[N1 <: Number[S, N1], N2 <: Number[S, N2]](left: N1, right: N2): Seq[Int] =
    zipWith(left.digits, right.digits, _ + _)

  final def subtract[N1 <: Number[S, N1], N2 <: Number[S, N2]](left: N1, right: N2): Seq[Int] =
    zipWith(left.digits, right.digits, _ - _)

  final def roundTo[N <: Number[S, N]](number: N, length: Int): Seq[Int] = {
    require(length >= 0)

    def forDigit(digit: Int, position: Int, range: Int): (Int, Int) =
      if (position < length) (0, digit)
      else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0)

    transform(normal(number), forDigit, (digit: Int) => digit)
  }

  final def toRational[N <: Number[S, N]](number: N): BigRational = to[BigRational](
    number.digits,
    (digit: Int, denominator: BigInt) => BigRational(digit, denominator),
    _ + _
  )

  final def toDouble[N <: Number[S, N]](number: N): Double = to[Double](
    number.digits,
    (digit: Int, denominator: BigInt) => digit.toDouble/denominator.bigInteger.longValueExact(),
    _ + _
  )

  private final def to[T](digits: Seq[Int], forDigit: (Int, BigInt) => T, plus: (T, T) => T): T = {
    val zeroDenominator: BigInt = BigInt(1)
    digits.tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }
    .foldLeft[(T, BigInt)]((forDigit(digits.head, zeroDenominator), zeroDenominator)) {
      case ((acc: T, denominator: BigInt), (digit: Int, range: Int)) =>
        val newDenominator: BigInt = denominator*range
        (plus(acc, forDigit(digit, newDenominator)), newDenominator)
    }._1
  }

  final def fromRational(value: BigRational, length: Int = defaultLength): Seq[Int] =
    from[BigRational](
      value.signum,
      value.abs,
      length,
      _.wholeAndFraction,
      _ * _,
      BigRational.round
    )

  final def fromDouble(value: Double, length: Int = defaultLength): Seq[Int] = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      (whole.toInt, what - whole)
    }

    def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt

    from[Double](
      math.signum(value).toInt,
      math.abs(value),
      length,
      wholeAndFraction,
      _ * _,
      round
    )
  }

  // This is an instance of a specialized unfold with an initiator, unfolder and terminator
  // (but we don't have even a simple unfold in the standard library)
  private final def from[T](
    signum: Int,
    value: T,
    length: Int,
    wholeAndFraction: T => (Int, T),
    mult: (T, Int) => T,
    round: (Int, T) => Int): Seq[Int] =
  {
    val (rawDigits: Seq[(Int, T)], (lastDigit: Int, lastReminder: T))  =
      (0 until length).map(range)
        .foldLeft((Seq.empty[(Int, T)], wholeAndFraction(value))) {
          case ((acc, (digit: Int, reminder: T)), range: Int) =>
            (acc :+ (digit, reminder), wholeAndFraction(mult(reminder, range)))
        }
    val digits: Seq[Int] = rawDigits.map(_._1)
    val result: Seq[Int] = digits.head +: digits.tail :+ round(lastDigit, lastReminder)
    if (signum != -1) result else negate(result)
  }

  final def toString[N <: Number[S, N]](number: N, length: Int): String = {
    def digitToString(defaultSign: String)(pair: (Int, Option[String])): String = {
      val (digit: Int, sign: Option[String]) = pair
      math.abs(digit) + sign.getOrElse(defaultSign)
    }

    val simpleDigits: Seq[Int] = simple(number)
    val digitsWithSigns: Seq[(Int, Option[String])] =
      simpleDigits.tail.padTo(length, 0).zipWithIndex.map {
        case (digit, position) => (digit, sign(position))
      }
    val tailResult: Seq[String] =
      if (digitsWithSigns.isEmpty) Seq.empty
      else digitsWithSigns.init.map(digitToString(",")) :+ digitToString("")(digitsWithSigns.last)

    val result: Seq[String] = (math.abs(simpleDigits.head) + headSign) +: tailResult

    (if (isNegative(simpleDigits)) "-" else "") + result.mkString
  }

  final def canonical[N <: Number[S, N]](number: N): Seq[Int] = {
    val normalDigits: Seq[Int] = normal(number)
    val result: Seq[Int] = positive(normalDigits)
    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  final def simple[N <: Number[S, N]](number: N): Seq[Int] = {
    val normalDigits: Seq[Int] = normal(number)
    if (isNegative(normalDigits)) negative(normalDigits) else positive(normalDigits)
  }

  final def normal[N <: Number[S, N]](number: N): Seq[Int] = normal(number.digits)
  final def normal(digits: Seq[Int]): Seq[Int] = transform(digits, normalDigit, normalHead)

  protected final def normalDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    (digit / digitRange, digit % digitRange)

  protected def normalHead(value: Int): Int

  private final def positive(digits: Seq[Int]): Seq[Int] =
    transform(digits, positiveDigit, positiveHead)

  protected final def positiveDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit >= 0) (0, digit) else (-1, digit + digitRange)

  protected def positiveHead(value: Int): Int

  private final def negative(digits: Seq[Int]): Seq[Int] =
    transform(digits, negativeDigit, negativeHead)

  protected final def negativeDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit <= 0) (0, digit) else (1, digit - digitRange)

  protected def negativeHead(value: Int): Int

  private final def transform(
    digits: Seq[Int],
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Seq[Int] = {
    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex :\(0, Seq.empty[Int])) {
      case ((digit: Int, position: Int), (carry: Int, result: Seq[Int])) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }

  private final def zipWith(
    left: Seq[Int],
    right: Seq[Int],
    operation: (Int, Int) => Int
  ): Seq[Int] =
    left.zipAll(right, 0, 0).map(operation.tupled)
}
