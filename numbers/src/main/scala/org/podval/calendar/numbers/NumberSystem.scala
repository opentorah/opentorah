package org.podval.calendar.numbers

import NumberSystem.RawNumber

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  final def newPoint(raw: RawNumber): S#Point = createPoint(normalize(raw))

  protected def createPoint(raw: RawNumber): S#Point

  type Interval <: IntervalBase[S]

  final def newInterval(raw: RawNumber): S#Interval = createInterval(normalize(raw))

  protected def createInterval(raw: RawNumber): S#Interval

  /**
    *
    * @param position
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSuffix: String

  val suffixPartial: PartialFunction[Int, String]

  def suffix(position: Int): Option[String] = suffixPartial.lift(position)

  final def multiplier(position: Int): Int /*TODO BigInt*/ = (1 to position).map(range).product

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  private final def normalize(raw: RawNumber): RawNumber = {
    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, position) = elem
      val (carry, result) = acc
      val value = digit + carry
      val range = this.range(position)
      val (quotient, reminder) = (value / range, value % range)
      val (carry_, digit_) =
        if (value >= 0) (quotient, reminder)
        else (quotient - 1, reminder + range)

      (carry_, digit_ :: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val carriedHead = correctHeadDigit(head + headCarry)
      val carriedNegative = carriedHead < 0
      val newHead = if (!carriedNegative) carriedHead else -carriedHead

      (carriedNegative, newHead)
    }

    val (negative, digits) = raw
    val (headCarry, newTail) = (digits.tail.zipWithIndex :\(0, List.empty[Int]))(step)
    val (carriedNegative, newHead) = headStep(digits.head, headCarry)

    val newNegative = if (negative) !carriedNegative else carriedNegative
    val newDigits = newHead :: newTail

    // Ensure that digits are within appropriate ranges
    newDigits.foreach(digit => require(digit >= 0, s"$digit must be non-negative"))

    checkHeadDigit(newHead)

    newTail.zipWithIndex.foreach { case (digit, position) =>
      val range = this.range(position)
      require(digit < range, s"$digit must be less than $range")
    }

    (newNegative, newDigits.reverse.dropWhile(_ == 0).reverse) // no dropWhileRight :)
  }

  final def fromDouble(value: Double, length: Int): RawNumber = {
    val negative = value < 0
    val absValue = if (!negative) value else -value

    val digits = absValue +: (1 to length).toList.map (
      position => (absValue % (1.0d / multiplier(position-1))) / (1.0d / multiplier(position))
    )

    (negative, (digits.init map (math.floor(_).toInt)) :+ math.round(digits.last).toInt)
  }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])
}
