package org.opentorah.astronomy

import org.opentorah.numbers.BigRational
import Angles.{Digit, Rotation}
import Rotation.Interval
import Days2Rotation.Days
import scala.annotation.tailrec

object Days2Rotation:

  enum Days(val number: Int) derives CanEqual:
    case One extends Days(1)
    case Ten extends Days(10)
    case Hundred extends Days(100)
    case Thousand extends Days(1000)
    case TenThousand extends Days(10000)
    case Month extends Days(29)
    case Year extends Days(354)

  object Days:
    val all: Seq[Days] = Seq(One, Ten,  Hundred, Thousand, TenThousand, Month, Year)

  private val findInsideMaxLength: Int = 6

  enum IntervalRelationship derives CanEqual:
    case Intersect(intersection: Interval)
    case Separated(separator: Rotation)
    case DoNotIntersect

  object IntervalRelationship:
    def get(a: Interval, b: Interval, length: Int): IntervalRelationship =
      def maybeSeparated(to: Rotation, from: Rotation): IntervalRelationship =
        require(to < from)
        require(to.roundTo(length) == to)
        require(from.roundTo(length) == from)
        val separator: Rotation = to + Rotation.oneAtPosition(length)
        if separator < from
        then IntervalRelationship.Separated(separator)
        else IntervalRelationship.DoNotIntersect

      if a.to < b.from then maybeSeparated(a.to, b.from) else
      if b.to < a.from then maybeSeparated(b.to, a.from) else
        val from: Rotation = if a.from < b.from then b.from else a.from
        val to  : Rotation = if a.to   > b.to   then b.to   else a.to
        IntervalRelationship.Intersect(Interval(from, to))

abstract class Days2Rotation(val name: String, values: (Days, String)*) extends MapTable.RotationMapTable[Days](values*):
  private def isOneDefined: Boolean = !value(Days.One).isZero
  private def smallestDefined: Days = if isOneDefined then Days.One else Days.Ten
  final def all: Seq[Days] = if isOneDefined then Days.all else Days.all.tail

  def precision(days: Days): Angles.Digit = Digit.SECONDS

  def calculate(from: Rotation, multiplier: Int, precision: Int): Rotation =
    (from * multiplier).canonical.roundTo(precision)

  private def calculate(from: Rotation, to: Days, multiplier: Int): Rotation =
    calculate(from, multiplier, precision(to).position)

  final def calculate(from: Rotation, to: Days): Rotation =
    calculate(from, to, to.number)

  final def isReconstructable(from: Rotation, days: Days): Boolean = calculate(from, days) == value(days)
  
  final def calculate(to: Days): Option[Rotation] =
    val from: Days = smallestDefined
    if (to.number == from.number) || (to.number % from.number != 0) then None else
      val multiplier: Int = to.number/from.number
      Some(calculate(value(from), to, multiplier))

  final def calculate(days: Int): Rotation =
    val tenThousands: Int = days / 10000
    val thousands: Int = (days % 10000) / 1000
    // TODO year!
    val hundreds: Int = (days % 1000) / 100
    val lessThanHundred: Int = days % 100
    // TODO month!
    val tens: Int = (days % 100) / 10
    val ones: Int = days % 10

    value(Days.TenThousand) * tenThousands +
    value(Days.Thousand) * thousands +
    value(Days.Hundred) * hundreds +
    (if lessThanHundred == 29 then value(Days.Month) else value(Days.Ten) * tens + value(Days.One) * ones)

  final lazy val exactMin: Map[Days, (Interval, Int)] =
    Map.from(
      for days <- Days.all yield
        val exactRotation: ExactRotation = mkExactRotation(days)
        val (interval: Interval, length: Int) = exactRotation.findInside(precision(days).position).get
        days -> (exactRotation.expand(interval, length, length), length)
    )

  final lazy val exactMinLength: Int = exactMin.values.map(_._2).max

  final def exact(maxLength: Int): Map[Days, Interval] =
    require(maxLength >= exactMinLength)

    Map.from(
      for days <- Days.all yield
        val (interval: Interval, length: Int) = exactMin(days)
        days -> mkExactRotation(days).expand(interval, length, maxLength)
    )

  private def mkExactRotation(days: Days): ExactRotation = ExactRotation(
    days = days.number,
    precision = precision(days).position,
    value = value(days)
  )

  final class ExactRotation(
    days: Int,
    precision: Int,
    value: Rotation
  ):
    private def reconstruct(from: Rotation): Rotation = calculate(from, days, precision)
    private def isInside(from: Rotation): Boolean = reconstruct(from) == value

    @tailrec def findInside(length: Int): Option[(Interval, Int)] =
      val start: Rotation = if value.isZero then Rotation.oneAtPosition(length) else
        // Note: with thanks to Tzikuni for pointing out that full periods need to be added before the division
        val inDays: BigRational = Days2Rotation.this.value(smallestDefined).toRational * BigRational(days, smallestDefined.number)
        val fullCircles: Rotation = Angles.period*(inDays/Angles.period.toRational).whole
        (value + fullCircles) / (days, length)

      val decrease: Boolean = reconstruct(start) > value

      @tailrec def findInsideAt(current: Rotation): Option[Rotation] =
        val reconstructed: Rotation = reconstruct(current)
        val comparison: Int = reconstructed.compare(value)
//        println(s"days=$days precision=$precision value=$value decrease=$decrease current=$current comparison=$comparison")
        if comparison == 0 then Some(current) else
          val overshot: Boolean = if decrease then comparison < 0 else comparison > 0
          if overshot then None else
            val step: Rotation = Rotation.oneAtPosition(length)
            val next: Rotation = current + (if decrease then -step else step)
            if next <= Rotation.zero then None else findInsideAt(next)

      val found: Option[Rotation] = findInsideAt(start)
//      println(s"found($start)=$found")
      found match
        case Some(result) => Some((Interval(result, result), length))
        case None => if length >= Days2Rotation.findInsideMaxLength then None else findInside(length + 1)

    @tailrec def expand(current: Interval, length: Int, maxLength: Int): Interval =
      def expandAt(interval: Interval): Interval =
        val step: Rotation = Rotation.oneAtPosition(length)

        @tailrec def loop(current: Rotation, step: Rotation): Rotation =
          require(isInside(current))
          val next: Rotation = current + step
          if !isInside(next) then current else loop(next, step)

        require(isInside(interval.from))
        require(isInside(interval.to))
        val result: Interval = Interval(
          loop(interval.from, step = -step),
          loop(interval.to, step = step)
        )
        require(isInside(result.from))
        require(!isInside(result.from - step))
        require(isInside(result.to))
        require(!isInside(result.to + step))
        result

      val next: Interval = expandAt(current)
      if length >= maxLength then next else expand(next, length + 1, maxLength)

