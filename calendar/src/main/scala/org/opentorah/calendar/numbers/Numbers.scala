package org.opentorah.calendar.numbers

trait Numbers[S <: Numbers[S]] { this: S =>

  type NumbersMemberType <: NumbersMember[S]

  type Point <: PointNumber[S]

  type PointCompanionType <: PointCompanion[S]

  val Point: PointCompanionType

  type Vector <: VectorNumber[S]

  type VectorCompanionType <: VectorCompanion[S]

  val Vector: VectorCompanionType

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
    def mult(acc: BigInt, tail: Seq[Int]): Seq[BigInt] = tail.toList match {
      case Nil => Seq.empty
      case r :: rs => acc +: mult(acc * r, rs)
    }

    mult(BigInt(1), ranges :+ 0)
  }

  val Digit: DigitsDescriptor

  private[numbers] final def to[T: Convertible](digits: Seq[Int]): T =
    digits zip denominators.take(digits.length) map (Convertible[T].div _).tupled reduce Convertible[T].plus

  // this can probably be done with digit(i) = value*denominators(i).whole%denominator(i) - but will it be less precise?
  private[numbers] final def from[T : Convertible](value: T, length: Int): Seq[Int] = {
    val (digits: Seq[Int], lastReminder /*: T*/) =
      ranges.take(length).foldLeft((Seq.empty[Int], Convertible[T].abs(value))) {
        case ((acc: Seq[Int], reminder /*: T*/), range: Int) =>
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
    * @param number  to convert to String
    * @param length  desired number of digits after the point
    * @tparam N      flavor of the number
    * @return        String representation of the number
    */
  private[numbers] final def toString[N <: Number[S, N]](number: N, length: Int): String = {
    val digits: Seq[Int] = number.digits.padTo(length+1, 0)
    val signs: Seq[String] = Digit.signs.take(length+1).padTo(length, ",").padTo(length+1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map { case (digit: Int, sign: String) => math.abs(digit) + sign }
    (if (number.isNegative) "-" else "") + result.mkString
  }

  private[numbers] final def roundTo(digits: Seq[Int], length: Int): Seq[Int] = {
    require(length >= 0)

    transform(
      digits = digits,
      forDigit = (digit: Int, position: Int, range: Int) =>
        if (position < length) (0, digit)
        else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0),
      forHead = identity
    )
  }

  private[numbers] final def normalize(digits: Seq[Int], isCanonical: Boolean): Seq[Int] =  {
    def t(
      digits: Seq[Int],
      forDigit: (/* digit: */ Int, /* digitRange: */ Int) => (Int, Int)
    ): Seq[Int] = transform(
      digits,
      (digit: Int, position: Int, digitRange: Int) => forDigit(digit, digitRange),
      (headDigit: Int) =>
        if (!isCanonical) headDigit
        else headRangeOpt.fold(headDigit){ headRange: Int => forDigit(headDigit,headRange)._2 }
    )

    // fit all digits within their ranges
    val normalDigits: Seq[Int] = t(
      digits = if (digits.isEmpty) Seq(0) else digits,
      forDigit = (digit: Int, digitRange: Int) => (digit / digitRange, digit % digitRange)
    )

    // determine the sign of the result
    val willBePositive: Boolean = (signum(normalDigits) >= 0) || (isCanonical && headRangeOpt.isDefined)
    val sign: Int = if (willBePositive) 1 else -1

    // make all digits of the same sign
    val result: Seq[Int] = t(
      digits = normalDigits,
      forDigit = (digit: Int, digitRange: Int) =>
        if ((digit == 0) || (math.signum(digit) == sign)) (0, digit) else (-sign, digit + sign * digitRange)
    )

    // drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  private[numbers] final def signum(digits: Seq[Int]): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)

  private final def transform(
    digits: Seq[Int],
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Seq[Int] = {
    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex.foldRight(0, Seq.empty[Int])) {
      case ((digit: Int, position: Int), (carry: Int, result: Seq[Int])) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }
}
