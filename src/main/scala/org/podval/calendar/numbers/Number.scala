package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.{BasicNumber, RawNumber}

// TODO once this is split into a separate file, rename T to S (it is T now to avoid shadowing)
abstract class Number[T <: NumberSystem[T], N <: Number[T, N]](raw: RawNumber)
  extends BasicNumber with Ordered[N]
{ this: N =>
  def numberSystem: T

  protected final def newPoint(raw: RawNumber): T#Point = numberSystem.newPoint(raw)
  protected final def newInterval(raw: RawNumber): T#Interval = numberSystem.newInterval(raw)

  // TODO rename?
  protected def newN(raw: RawNumber): N

  protected final def maxLength = numberSystem.maxLength
  protected final def ranges = numberSystem.ranges
  protected final def signs = numberSystem.signs
  protected final def divisors = numberSystem.divisors

  final override def negative: Boolean = raw._1

  final override def digits: List[Int] = raw._2

  final def head: Int = digits.head

  final def tail: List[Int] = digits.tail

  // TODO is this correct - or should it be `digits.length`?
  final def length: Int = tail.length

  final def digit(n: Int): Int = {
    require(0 <= n && n <= maxLength)
    if (length >= n) digits(n) else 0
  }

  final def digit(n: Int, value: Int): N = {
    require(0 <= n && n <= maxLength)
    newN(negative, digits.padTo(n + 1, 0).updated(n, value))
  }

  protected final def plusMinus(operationNegation: Boolean, that: BasicNumber): RawNumber = {
    val sameSign = this.negative == that.negative
    val operationSelector = if (operationNegation) !sameSign else sameSign
    val operation: (Int, Int) => Int = if (operationSelector) _ + _ else _ - _
    (negative, zip(that).map(operation.tupled))
  }

  // TODO add rounding tests
  final def roundTo(n: Int): N = {
    require(n >= 0)

    val (more_, toRound) = tail splitAt n
    val tail_ = {
      if (more_.isEmpty) more_
      else {
        val toRoundWithRange = toRound zip ranges.drop(n)
        val carry = (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
        more_.init :+ (more_.last + carry)
      }
    }

    newN(negative, head +: tail_)
  }

  final def toDouble: Double =
    (if (negative) -1 else +1) * (head + ((tail zip divisors) map lift(_ / _)).sum)

  private[this] def zip(that: BasicNumber): List[(Int, Int)] = this.digits zipAll(that.digits, 0, 0)

  // TODO why can't I inline .tupled?
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled

  // TODO: padding; cutting off 0; more flavours...
  protected final def toSignedString: String = {
    val tokens = digits map (_.toString) zip signs flatMap (p => List(p._1, p._2))
    val result = (if (length <= 3) tokens else tokens.init).mkString
    (if (negative) "-" else "") + result
  }

  override def toString: String = toSignedString

  final override def hashCode: Int = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode

  final def compare(that: N): Int = {
    if (this.negative == that.negative) {
      val result = zip(that) map lift(_.compare(_)) find (_ != 0) getOrElse 0
      if (!this.negative) result else -result
    } else {
      if (!that.negative) +1 else -1
    }
  }

  final override def equals(other: Any): Boolean =
  // TODO deal with the "erasure" warning
    if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0

}
