package org.podval.calendar.numbers

import NumberSystem.RawNumber

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  def createPoint(raw: RawNumber): Point

  val Point: PointCompanion[S]

  type Interval <: IntervalBase[S]

  def createInterval(raw: RawNumber): Interval

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

  def sign(position: Int): Option[String] = signPartial.lift(position)

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  final def zipWithRanges(tail: Seq[Int]): Seq[(Int, Int)] =
    tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }
}


object NumberSystem {
  type RawNumber = (Boolean, Seq[Int])

  final def signum(negative: Boolean): Int = if (negative) -1 else +1
}
