package org.opentorah.numbers

import scala.annotation.tailrec

// To make clear all the knots-tying in Numbers, let's put operations on Digits in here and in HasDigits.
trait NumbersBase:

  /**
   * Maximum number of digits after the dot.
   *
   * @return maximum number of digits after the dot
   */
  def maxLength: Int

  def headRangeOpt: Option[Int]

  protected def digitSigns: Seq[String]

  protected def digitSignDefault: String

  /**
   *
   * @param position within the tail
   * @return positive, even number
   */
  def range(position: Int): Int

  private[numbers] final lazy val ranges: Seq[Int] = (0 until maxLength).map(range)

  private lazy val denominators: Seq[BigInt] =
    @tailrec def loop(acc: BigInt, result: Seq[BigInt], tail: Digits): Seq[BigInt] = tail.toList match
      case Nil => result.reverse
      case r :: rs => loop(acc * r, acc +: result, rs)

    loop(BigInt(1), Seq.empty, ranges :+ 0)

  /** Convert a number to String.
   *
   * If length specified is bugger than the number of digits after the point in the number,
   * missing positions are assumed to have 0s; if the length is smaller, some digits will not
   * be shown.
   * Signs (like 'ʰ' for hours or '°' for degrees) are inserted after digits.
   * If no sign is configured for a position, digitSignDefault is used - except for the last digit,
   * where no sign is appended in such a case.
   *
   * @param length desired number of digits after the point
   * @return String representation of the number
   */
  final protected def toStringDigits(hasDigits: HasDigits, length: Int): String =
    val digits: Digits = hasDigits.digits.padTo(length + 1, 0)
    val signs: Seq[String] = digitSigns.take(length + 1).padTo(length, digitSignDefault).padTo(length + 1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map ((digit: Int, sign: String) => math.abs(digit).toString + sign)
    (if HasDigits.signumDigits(digits) < 0 then "-" else "") + result.mkString

  final protected def fromStringDigits(string: String): Digits =
    val stringTrimmed: String = string.trim
    val isNegative: Boolean = stringTrimmed.startsWith("-")

    @tailrec def loop(result: Digits, rest: String): Digits =
      def parseDigit(digitString: String): Int =
        val digit: Int = digitString.trim.toInt
        if isNegative then -digit else digit

      if rest.isEmpty then result else
        if result.length > maxLength then throw IllegalArgumentException(s"Too many digits: got $result, remaining: $rest")
        val length: Int = result.length
        val signIsDefault: Boolean = length >= digitSigns.length
        val sign: String = if signIsDefault then digitSignDefault else digitSigns(length)
        val signIndex: Int = rest.indexOf(sign)
        val noSign: Boolean = signIndex == -1
        if noSign then
          if !signIsDefault then throw IllegalArgumentException(s"Missing digit sign $sign in $rest")
          loop(result :+ parseDigit(rest), "")
        else
          loop(result :+ parseDigit(rest.substring(0, signIndex)), rest.substring(signIndex + sign.length))

    loop(Seq.empty, if isNegative then stringTrimmed.substring(1) else stringTrimmed)

  private def transform(
    digits: Digits,
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Digits =
    val (headCarry: Int, newTail: Digits) = digits.tail.zipWithIndex.foldRight(0, Seq.empty[Int]) {
      case ((digit: Int, position: Int), (carry: Int, result: Digits)) =>
        val (resultCarry: Int, resultDigit: Int) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail

  final protected def roundToDigits(hasDigits: HasDigits, length: Int): Digits =
    require(length >= 0)
    transform(
      digits = hasDigits.digits,
      forDigit = (digit: Int, position: Int, range: Int) =>
        if position < length then (0, digit)
        else (if math.abs(digit) >= range / 2 then math.signum(digit) else 0, 0),
      forHead = identity
    )

  final protected def convertTo[T: Convertible](hasDigits: HasDigits): T = convertTo[T](hasDigits.digits)
  final protected def convertTo[T: Convertible](digits: Digits)(using convertible: Convertible[T]): T =
    digits.zip(denominators.take(digits.length))
      .map(convertible.div.tupled)
      .reduce(convertible.plus)

  // this can probably be done with digit(i) = value*denominators(i).whole%denominator(i) - but will it be less precise?
  final protected def convertFrom[T: Convertible](value: T, length: Int)(using convertible: Convertible[T]): Digits =
    val (digits: Digits, lastRemainder: T) = ranges.take(length)
      .foldLeft((Seq.empty[Int], convertible.abs(value))) { case ((acc: Digits, remainder: T), range: Int) =>
        val (whole: Int, fraction: T) = convertible.wholeAndFraction(remainder)
        (acc :+ whole, convertible.mult(fraction, range))
      }

    (digits :+ convertible.round(lastRemainder)).map(convertible.signum(value) * _)

  final protected def getDigits(digits: Digits, isCanonical: Boolean = false): Digits =
    def t(
      digits: Digits,
      forDigit: ( /* digit: */ Int, /* digitRange: */ Int) => (Int, Int)
    ): Digits = transform(
      digits,
      (digit: Int, _ /* TODO position - unused! */ : Int, digitRange: Int) => forDigit(digit, digitRange),
      (headDigit: Int) =>
        if !isCanonical then headDigit
        else headRangeOpt.fold(headDigit)((headRange: Int) => forDigit(headDigit, headRange)._2)
    )

    // fit all digits within their ranges
    val normalDigits: Digits = t(
      digits = if digits.isEmpty then Seq(0) else digits,
      forDigit = (digit: Int, digitRange: Int) => (digit / digitRange, digit % digitRange)
    )

    // determine the sign of the result
    val willBePositive: Boolean = (HasDigits.signumDigits(normalDigits) >= 0) || (isCanonical && headRangeOpt.isDefined)
    val sign: Int = if willBePositive then 1 else -1

    // make all digits of the same sign
    val result: Digits = t(
      digits = normalDigits,
      forDigit = (digit: Int, digitRange: Int) =>
        if (digit == 0) || (math.signum(digit) == sign) then (0, digit) else (-sign, digit + sign * digitRange)
    )

    // drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    result.head +: result.tail.reverse.dropWhile(_ == 0).reverse
