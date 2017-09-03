package org.podval.calendar.numbers

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  def createPoint(negative: Boolean, digits: Seq[Int]): Point

  val Point: PointCompanion[S]

  type Interval <: IntervalBase[S]

  def createInterval(negative: Boolean, digits: Seq[Int]): Interval

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
  final def signum(negative: Boolean): Int = if (negative) -1 else +1
}
