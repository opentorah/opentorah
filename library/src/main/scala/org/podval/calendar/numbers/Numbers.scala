package org.podval.calendar.numbers

trait Numbers[S <: Numbers[S]] { this: S =>

  type NumbersMemberType <: NumbersMember[S]

  type Point <: PointBase[S]

  type PointCompanionType <: NumberCompanion[S, S#Point]

  val Point: PointCompanionType

  type Vector <: VectorBase[S]

  type VectorCompanionType <: NumberCompanion[S, S#Vector]

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
    val digits: Seq[Int] = normalize(number.digits, isCanonical = false).padTo(length+1, 0)
    val signs: Seq[String] = Digit.signs.take(length+1).padTo(length, ",").padTo(length+1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map { case (digit: Int, sign: String) => math.abs(digit) + sign }
    (if (number.isNegative) "-" else "") + result.mkString
  }

  private[numbers] final def normalize(digits: Seq[Int], isCanonical: Boolean): Seq[Int] =  {
    val normalDigits: Seq[Int] = transform(
      digits = if (digits.isEmpty) Seq(0) else digits,
      forDigit = normalDigit,
      forHead = (value: Int) => if (isCanonical) headDigit(normalDigit, value) else value
    )
    val isPositive: Boolean = isCanonical || (signum(normalDigits) >= 0)
    val forDigit: (Int, Int, Int) => (Int, Int) = signedDigit(if (isPositive) 1 else -1)
    val result: Seq[Int] = transform(
      digits = normalDigits,
      forDigit = forDigit,
      forHead = (value: Int) => headDigit(forDigit, value)
    )
    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
  }

  private[numbers] final def signum(digits: Seq[Int]): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)

  private def normalDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    (digit / digitRange, digit % digitRange)

  private def signedDigit(sign: Int)(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if ((digit == 0) || (math.signum(digit) == sign)) (0, digit) else (-sign, digit + sign*digitRange)

  private def headDigit(f: (Int, Int, Int) => (Int, Int), value: Int): Int =
    headRangeOpt.fold(value){ headRange: Int => f(value, -1, headRange)._2 }

  private[numbers] final def roundTo(digits: Seq[Int], length: Int): Seq[Int] = {
    require(length >= 0)

    def forDigit(digit: Int, position: Int, range: Int): (Int, Int) =
      if (position < length) (0, digit)
      else (if (math.abs(digit) >= range / 2) math.signum(digit) else 0, 0)

    transform(normalize(digits, isCanonical = false), forDigit, (digit: Int) => digit)
  }

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
}
