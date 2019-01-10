package org.podval.calendar.numbers

trait Numbers[S <: Numbers[S]] { this: S =>

  type Point <: PointBase[S]

  val Point: PointCompanion[S]

  type Vector <: VectorBase[S]

  val Vector: VectorCompanion[S]

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

  final lazy val ranges: Seq[Int] = (0 until maxLength).map(range)

  final lazy val denominators: Seq[BigInt] = {
    def mult(acc: BigInt, tail: Seq[Int]): Seq[BigInt] = tail.toList match {
      case Nil => Seq.empty
      case r :: rs => acc +: mult(acc * r, rs)
    }

    mult(BigInt(1), ranges :+ 0)
  }

  val Digit: DigitsDescriptor

  final def to[T: Convertible](digits: Seq[Int]): T =
    digits zip denominators.take(digits.length) map (Convertible[T].div _).tupled reduce Convertible[T].plus

  final def from[T : Convertible](value: T, length: Int): Seq[Int] = {
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
  final def toString[N <: Number[S, N]](number: N, length: Int): String = {
    val digits: Seq[Int] = number.simple.digits.padTo(length+1, 0)
    val signs: Seq[String] = Digit.signs.take(length+1).padTo(length, ",").padTo(length+1, "")

    // ignore the signums of digits: all digits have the same signum, which we reflect in the overall result
    val result: Seq[String] = (digits zip signs) map { case (digit: Int, sign: String) => math.abs(digit) + sign }
    (if (number.isNegative) "-" else "") + result.mkString
  }

  final def transform(
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
