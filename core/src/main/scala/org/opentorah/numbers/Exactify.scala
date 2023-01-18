package org.opentorah.numbers

import scala.annotation.tailrec

final class Exactify(val numbers: Numbers.Periodic)(
  small: numbers.Vector,
  multiplier: Int,
  round: numbers.Digit,
  big: numbers.Vector
):

  import numbers.Vector
  import Vector.Interval

  def calculate(arg: Vector): Vector = (arg*multiplier).canonical.roundTo(round)

  private def isInside(value: Vector): Boolean = calculate(value) == big

  private def stepForLength(length: Int): Vector = Vector(0).set(length, 1)

  private def findInside(maxLength: Int): Option[(Interval, Int)] =
    @tailrec def findInsideAt(current: Vector, decrease: Boolean, length: Int): Option[Vector] =
      val calculated: Vector = calculate(current)
      if calculated == big then Some(current) else
        val overshot: Boolean = if decrease then calculated < big else calculated > big
        if overshot then None else
          val step: Vector = stepForLength(length)
          findInsideAt(current + (if decrease then -step else step), decrease, length)

    @tailrec def loop(length: Int): Option[(Interval, Int)] = findInsideAt(small, calculate(small) > big, length) match
      case Some(result) => Some((Interval(result, result), length))
      case None => if length >= maxLength then None else loop(length+1)

    loop(round.position)

  private def expand(interval: Interval, length: Int): Interval =
    val step: Vector = stepForLength(length)

    @tailrec def loop(current: Vector, decrease: Boolean): Vector =
      require(isInside(current))
      val next: Vector = current + (if decrease then -step else step)
      if !isInside(next) then current else loop(next, decrease)

    require(isInside(interval.from))
    require(isInside(interval.to  ))
    val result: Interval = Interval(
      loop(interval.from, decrease = true ),
      loop(interval.to  , decrease = false)
    )
    require(isInside(result.from))
    require(isInside(result.to  ))
    require(!isInside(result.from-step))
    require(!isInside(result.to  +step))
    result

  def find(maxLength: Int): Option[(Interval, Int)] =
    findInside(maxLength).map((interval, length) =>
      (expand(interval, length), length)
    )

  def findPrecise(maxLength: Int): Option[Interval] =
    @tailrec def loop(current: Interval, length: Int): Interval =
      val next: Interval = expand(current, length)
      if length >= maxLength then next else loop(next, length + 1)

    findInside(maxLength).map((interval, length) =>
      loop(interval, length)
    )
