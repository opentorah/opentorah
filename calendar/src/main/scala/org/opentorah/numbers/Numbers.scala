package org.opentorah.numbers

import scala.language.implicitConversions

trait Numbers[S <: Numbers[S]] {
  this: S =>

  type Point <: PointNumber[S]

  val Point: PointCompanion[S]

  type Vector <: VectorNumber[S]

  val Vector: VectorCompanion[S]

  def headRangeOpt: Option[Int]

  /**
   * Maximum number of digits after the dot.
   *
   * @return maximum number of digits after the dot
   */
  def maxLength: Int

  /**
   *
   * @param position within the tail
   * @return positive, even number
   */
  def range(position: Int): Int

  private[numbers] final lazy val ranges: Seq[Int] = (0 until maxLength).map(range)

  private final lazy val denominators: Seq[BigInt] = {
    def mult(acc: BigInt, tail: Digits): Seq[BigInt] = tail.toList match {
      case Nil => Seq.empty
      case r :: rs => acc +: mult(acc * r, rs)
    }

    mult(BigInt(1), ranges :+ 0)
  }

  val Digit: DigitsDescriptor

  private[numbers] final def to[T: Convertible](digits: Digits): T =
    digits zip denominators.take(digits.length) map (Convertible[T].div _).tupled reduce Convertible[T].plus

  // this can probably be done with digit(i) = value*denominators(i).whole%denominator(i) - but will it be less precise?
  private[numbers] final def from[T: Convertible](value: T, length: Int): Digits = {
    val (digits: Digits, lastReminder /*: T*/) =
      ranges.take(length).foldLeft((Seq.empty[Int], Convertible[T].abs(value))) {
        case ((acc: Digits, reminder /*: T*/), range: Int) =>
          val (whole: Int, fraction /*: T*/) = Convertible[T].wholeAndFraction(reminder)
          (acc :+ whole, Convertible[T].mult(fraction, range))
      }

    (digits :+ Convertible[T].round(lastReminder)).map(Convertible[T].signum(value)*_)
  }

  /** Convert a number to String.
   *
   * If length specified is bugger than the number of digits after the point in the number,
   * missing positions are assumed to have 0s; if the length is smaller, some digits will not
   * be shown.
   * Signs (like 'h' for hours or '°' for degrees) are inserted after digits.
   * If no sign is configured for a position, ',' is used - except for the last digit,
   * where no sign is appended in such a case.
   *
   * @param number to convert to String
   * @param length desired number of digits after the point
   * @tparam N flavor of the number
   * @return String representation of the number
   */
  private[numbers] final def toString[N <: Number[S, N]](number: N, length: Int): String = {
    val digits: Digits = number.digits.padTo(length+1, 0)
    val signs: Seq[String] = Digit.signs.take(length+1).padTo(length, ",").padTo(length+1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map { case (digit: Int, sign: String) => math.abs(digit) + sign }
    (if (number.isNegative) "-" else "") + result.mkString
  }

  private[numbers] final def roundTo(digits: Digits, length: Int): Digits = {
    require(length >= 0)

    transform(
      digits = digits,
      forDigit = (digit: Int, position: Int, range: Int) =>
        if (position < length) (0, digit)
        else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0),
      forHead = identity
    )
  }

  private[numbers] final def normalize(digits: Digits, isCanonical: Boolean): Digits = {
    def t(
      digits: Digits,
      forDigit: (/* digit: */ Int, /* digitRange: */ Int) => (Int, Int)
    ): Digits = transform(
      digits,
      (digit: Int, _ /* TODO position - unused! */: Int, digitRange: Int) => forDigit(digit, digitRange),
      (headDigit: Int) =>
        if (!isCanonical) headDigit
        else headRangeOpt.fold(headDigit){ headRange: Int => forDigit(headDigit, headRange)._2 }
    )

    // fit all digits within their ranges
    val normalDigits: Digits = t(
      digits = if (digits.isEmpty) Seq(0) else digits,
      forDigit = (digit: Int, digitRange: Int) => (digit / digitRange, digit % digitRange)
    )

    // determine the sign of the result
    val willBePositive: Boolean = (signum(normalDigits) >= 0) || (isCanonical && headRangeOpt.isDefined)
    val sign: Int = if (willBePositive) 1 else -1

    // make all digits of the same sign
    val result: Digits = t(
      digits = normalDigits,
      forDigit = (digit: Int, digitRange: Int) =>
        if ((digit == 0) || (math.signum(digit) == sign)) (0, digit) else (-sign, digit + sign * digitRange)
    )

    // drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  private[numbers] final def signum(digits: Digits): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)

  private final def transform(
    digits: Digits,
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Digits = {
    val (headCarry: Int, newTail: Digits) = digits.tail.zipWithIndex.foldRight(0, Seq.empty[Int]) {
      case ((digit: Int, position: Int), (carry: Int, result: Digits)) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }

  // Ordering implicits (ignore bogus Idea warnings)

  implicit val pointOrdering: Ordering[Point] = (x: Point, y: Point) => x.compare(y)

  implicit def pointOrderingOps(lhs: Point): pointOrdering.Ops = pointOrdering.mkOrderingOps(lhs)

  implicit val vectorOrdering: Ordering[Vector] = (x: Vector, y: Vector) => x.compare(y)

  implicit def vectorOrderingOps(lhs: Vector): vectorOrdering.Ops = vectorOrdering.mkOrderingOps(lhs)
}
