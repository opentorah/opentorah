package org.opentorah.numbers

type Digits = Seq[Int]

trait HasDigits:
  def digits: Digits

  /** Returns number of digits after the `point`. */
  final def length: Int = digits.tail.length

  /** Returns digit at `position`. */
  final def get(position: Int): Int = if digits.length > position then digits(position) else 0

  final protected def setDigits(position: Int, value: Int): Digits =
    digits.padTo(position + 1, 0).updated(position, value)

  /** Returns the signum of this number: `-1` if it is negative, `1` if it is positive and `0` if it is neither. */
  final def signum: Int = HasDigits.signumDigits(digits)

  final protected def absDigits: Digits = digits.map(math.abs)

  final protected def negateDigits: Digits = digits.map(-_)

  final protected def multiplyDigits(n: Int): Digits = digits.map(_ * n)

  final protected def compareDigits(that: HasDigits): Int = zipWith(that.digits, _.compare(_)).find(_ != 0).getOrElse(0)

  final protected def addDigits(that: HasDigits): Digits = addDigits(that.digits)
  final protected def addDigits(that: Digits): Digits = zipWith(that, _ + _)

  final protected def subtractDigits(that: HasDigits): Digits = zipWith(that.digits, _ - _)

  private def zipWith(
    right: Digits,
    operation: (Int, Int) => Int
  ): Digits =
    digits.zipAll(right, 0, 0).map(operation.tupled)

object HasDigits:
  def signumDigits(digits: Digits): Int = digits.find(_ != 0).map(math.signum).getOrElse(0)
