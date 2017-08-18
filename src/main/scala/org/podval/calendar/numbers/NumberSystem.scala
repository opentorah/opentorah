package org.podval.calendar.numbers

trait NumberSystem[S <: NumberSystem[S]] { this: S =>
  import NumberSystem.RawNumber

  type Point <: PointBase[S]

  final def newPoint(raw: RawNumber): S#Point = createPoint(normalize(raw))

  def createPoint(raw: RawNumber): S#Point

  type Interval <: IntervalBase[S]

  final def newInterval(raw: RawNumber): S#Interval = createInterval(normalize(raw))

  def createInterval(raw: RawNumber): S#Interval

  val ranges: List[Int]

  ranges.foreach { range =>
    require(range > 0)
    require(range % 2 == 0)
  }

  val signs: List[String]

  require(signs.length == (ranges.length + 1))

  val maxLength: Int = ranges.length

  val divisors: List[Double] = ranges.inits.toList.reverse.tail.map(_.product.toDouble)

  // TODO move into the companion object by adding `ranges` parameter?
  private final def normalize(raw: RawNumber): RawNumber = {
    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, range) = elem
      val (carry, result) = acc
      val value = digit + carry
      val (quotient, reminder) = (value / range, value % range)
      val (carry_, digit_) = if (value >= 0) (quotient, reminder) else (quotient - 1, reminder + range)

      (carry_, digit_ :: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val carriedHead = correctHeadDigit(head + headCarry)
      val carriedNegative = carriedHead < 0
      val newHead = if (!carriedNegative) carriedHead else -carriedHead

      (carriedNegative, newHead)
    }

    val (negative, digits) = raw
    val (headCarry, newTail) = ((digits.tail zip ranges) :\(0, List.empty[Int]))(step)
    val (carriedNegative, newHead) = headStep(digits.head, headCarry)

    val newNegative = if (negative) !carriedNegative else carriedNegative
    val newDigits = newHead :: newTail

    // Ensure that digits are within appropriate ranges
    newDigits.foreach(digit => require(digit >= 0, "must be non-negative"))

    checkHeadDigit(newHead)

    (newTail zip ranges) foreach { case (digit, range) =>
      require(digit < range, "must be less than " + range)
    }

    (newNegative, newDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  // TODO this can also move into the companion object.
  final def fromDouble(value: Double, length: Int): RawNumber = {
    val negative = value < 0
    val absValue = if (!negative) value else -value

    val digits = absValue +: ((((1.0d :: divisors.init) zip divisors) take length) map { case (previous, current) =>
      (absValue % (1.0d / previous)) / (1.0d / current)
    })

    normalize(negative, (digits.init map (math.floor(_).toInt)) :+ math.round(digits.last).toInt)
  }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])

  // TODO I only introduced this because I couldn't figure out how to type plusMinus() and zip()
  // so that the calls to them compile :(
  // (I tried things like X <: Number[T, X] etc...)
  // It should go away!
  trait BasicNumber {
    def negative: Boolean
    def digits: List[Int]
  }
}
