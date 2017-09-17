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

  final def signum(digits: Seq[Int]): Int = nonZeroDigit(digits).map(math.signum).getOrElse(0)

  final def isZero(digits: Seq[Int]): Boolean = nonZeroDigit(digits).isEmpty

  final def isPositive(digits: Seq[Int]): Boolean = nonZeroDigit(digits).exists(_ > 0)

  final def isNegative(digits: Seq[Int]): Boolean = nonZeroDigit(digits).exists(_ < 0)

  private[this] def nonZeroDigit(digits: Seq[Int]): Option[Int] = normal(digits).find(_ != 0)

  final def get(digits: Seq[Int], position: Int): Int = {
    val normalDigits: Seq[Int] = normal(digits)
    if (normalDigits.length > position) normalDigits(position) else 0
  }

  final def set(digits: Seq[Int], position: Int, value: Int): Seq[Int] = {
    val normalDigits: Seq[Int] = normal(digits)
    normalDigits.padTo(position+1, 0).updated(position, value)
  }

  final def compare(left: Seq[Int], right: Seq[Int]): Int =
    zipWith(canonical(left), canonical(right), _ compare _).find (_ != 0) getOrElse 0

  // TODO should probably pipe through normal().
  final def abs(digits: Seq[Int]): Seq[Int] = digits.map(math.abs)

  final def negate(digits: Seq[Int]): Seq[Int] = digits.map(-_)

  final def add(left: Seq[Int], right: Seq[Int]): Seq[Int] = zipWith(left, right, _ + _)

  final def subtract(left: Seq[Int], right: Seq[Int]): Seq[Int] = zipWith(left, right, _ - _)

  // TODO test with negativity
  final def roundTo(digits: Seq[Int], length: Int): Seq[Int] = {
    require(length >= 0)

    val normalDigits: Seq[Int] = normal(digits)
    val (toRetain, toRound) = normalDigits.tail splitAt length
    val toRoundWithRange = toRound.zipWithIndex.map {
      case (digit, position) => (digit, range(length+position))
    }
    val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}

    if (toRetain.isEmpty) Seq(normalDigits.head + carry)
    else normalDigits.head +: toRetain.init :+ (toRetain.last + carry)
  }

  final def toRational(digits: Seq[Int]): BigRational = {
    def forDigit(digit: Int, denominator: BigInt): BigRational =
      BigRational(digit, denominator)
    to[BigRational](digits, forDigit, _ + _)
  }

  final def toDouble(digits: Seq[Int]): Double = {
    def forDigit(digit: Int, denominator: BigInt): Double =
      digit.toDouble/denominator.bigInteger.longValueExact()
    to[Double](digits, forDigit, _ + _)
  }

  private final def to[T](digits: Seq[Int], forDigit: (Int, BigInt) => T, plus: (T, T) => T): T = {
    val zeroDenominator: BigInt = BigInt(1)
    zipWithRanges(digits)
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
      val fraction: Double = what - whole
      (whole.toInt, fraction)
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

  final def toString(digits: Seq[Int], length: Int): String = {
    def digitToString(defaultSign: String)(pair: (Int, Option[String])): String = {
      val (digit: Int, sign: Option[String]) = pair
      math.abs(digit) + sign.getOrElse(defaultSign)
    }

    val simpleDigits: Seq[Int] = simple(digits)
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

  final def hashCode(digits: Seq[Int]): Int = (73 /: canonical(digits))((v, x) => 41 * v + x)

  final def canonical(digits: Seq[Int]): Seq[Int] = {
    val normalDigits: Seq[Int] = normal(digits)
    val result: Seq[Int] = positive(normalDigits)
    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  final def simple(digits: Seq[Int]): Seq[Int] = {
    val normalDigits: Seq[Int] = normal(digits)
    if (isNegative(normalDigits)) negative(normalDigits) else positive(normalDigits)
  }

  final def normal(digits: Seq[Int]): Seq[Int] = transform(digits, normalDigit, normalHead)

  protected final def normalDigit(digit: Int, digitRange: Int): (Int, Int) =
    (digit / digitRange, digit % digitRange)

  protected def normalHead(digit: Int): Int = digit

  private final def positive(digits: Seq[Int]): Seq[Int] =
    transform(digits, positiveDigit, positiveHead)

  protected final def positiveDigit(digit: Int, digitRange: Int): (Int, Int) =
    if (digit >= 0) (0, digit) else (-1, digit + digitRange)

  protected def positiveHead(digit: Int): Int = digit

  private final def negative(digits: Seq[Int]): Seq[Int] =
    transform(digits, negativeDigit, negativeHead)

  protected final def negativeDigit(digit: Int, digitRange: Int): (Int, Int) =
    if (digit < 0) (0, digit) else (1, digit - digitRange)

  protected def negativeHead(digit: Int): Int = digit

  private final def transform(digits: Seq[Int], forDigit: (Int, Int) => (Int, Int), forHead: Int => Int): Seq[Int] = {
    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex :\(0, Seq.empty[Int])) {
      case ((digit: Int, position: Int), (carry: Int, result: Seq[Int])) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }

  private final def zipWith(left: Seq[Int], right: Seq[Int], operation: (Int, Int) => Int): Seq[Int] =
    left.zipAll(right, 0, 0).map(operation.tupled)

  private final def zipWithRanges(digits: Seq[Int]): Seq[(Int, Int)] =
    digits.tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }
}
