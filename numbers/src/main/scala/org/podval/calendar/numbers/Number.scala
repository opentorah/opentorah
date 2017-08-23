package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]](raw: RawNumber) extends Ordered[N]
{ this: N =>
  def numberSystem: S

  protected final def newPoint(raw: RawNumber): S#Point = numberSystem.newPoint(raw)
  protected final def newInterval(raw: RawNumber): S#Interval = numberSystem.newInterval(raw)

  // TODO rename?
  protected def newN(raw: RawNumber): N

  final def negative: Boolean = raw._1

  final def digits: List[Int] = raw._2

  final def head: Int = digits.head

  final def tail: List[Int] = digits.tail

  // TODO is this correct - or should it be `digits.length`?
  final def length: Int = tail.length

  final def digit(n: Int): Int = {
    require(0 <= n && n <= numberSystem.maxLength)
    if (length >= n) digits(n) else 0
  }

  final def digit(n: Int, value: Int): N = {
    require(0 <= n && n <= numberSystem.maxLength)
    newN(negative, digits.padTo(n + 1, 0).updated(n, value))
  }

  protected final def add(negate: Boolean, that: Number[S, _]): RawNumber = {
    val sameSign = this.negative == that.negative
    val operationSelector = if (negate) !sameSign else sameSign
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
        val toRoundWithRange = toRound zip numberSystem.ranges.drop(n)
        val carry =
          (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
        more_.init :+ (more_.last + carry)
      }
    }

    newN(negative, head +: tail_)
  }

  final def toDouble: Double =
    (if (negative) -1 else +1) * (head + ((tail zip numberSystem.divisors) map lift(_ / _)).sum)

  private[this] def zip(that: Number[S, _]): List[(Int, Int)] =
    this.digits zipAll(that.digits, 0, 0)

  // TODO why can't I inline .tupled?
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled

  // TODO: padding; cutting off 0; more flavours...
  protected final def toSignedString: String = {
    // TODO detect and drop default "sign" after the last digit.
    val tokens = digits.zipWithIndex.flatMap {
      case (digit, index) => List(digit.toString, numberSystem.sign(index))
    }
    (if (negative) "-" else "") + tokens.mkString
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
    // TODO deal with the "erasure" warning; compare numberSystem...
    if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0

}
