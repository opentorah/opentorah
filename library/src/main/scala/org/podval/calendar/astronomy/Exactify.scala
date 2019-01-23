package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Digit, Rotation}

final class Exactify(small: Rotation, mult: Int, round: Int, big: Rotation) {

  def findFit: (Interval, Int) = findFit(round)

  def findFit(length: Int): (Interval, Int) = {
    val result = find(/*squeeze(*/enclose(length)/*, length)*/, length)
    require(fits(result._1))
    result
  }

  def find(interval: Interval, length: Int): (Interval, Int) =
    if (fits(interval)) (interval, length) else find(squeeze(interval, length+1), length+1)

  def enclose(length: Int): Interval = {
    val step: Rotation = stepForLength(length)

    var from: Rotation = small
    var to: Rotation = small

    while ((calculate(from) > big) && from.isPositive) from = from-step
    while  (calculate(to  ) < big)                     to   = to  +step

    val result = Interval(from, to)

    require(encloses(result))
    result
  }

  def squeeze(interval: Interval, length: Int): Interval = {
    require(encloses(interval))

    val step: Rotation = stepForLength(length)

    var from: Rotation = interval.from
    var to: Rotation = interval.to

    while ((from <= to) && (calculate(from) < big) && (calculate(from+step) <= big)) from = from+step
    while ((from <= to) && (calculate(to  ) > big) && (calculate(to  -step) >= big) && to.isPositive) to   = to  -step

    val result = Interval(from, to)

    require(encloses(result))
    result
  }

  def expand(interval: Interval, length: Int, toLength: Int): Interval =
    if (length == toLength) interval else expand(expand1(interval, length+1), length+1, toLength)

  def expand1(interval: Interval, length: Int): Interval = {
    require(fits(interval))

    val step: Rotation = stepForLength(length)

    var from: Rotation = interval.from
    var to: Rotation = interval.to

    while (calculate(from-step) == big) from = from-step
    while (calculate(to  +step) == big) to   = to  +step

    val result = Interval(from, to)

    require(fits(result))
    result
  }

  def calculate(arg: Rotation): Rotation = (arg*mult).canonical.roundTo(round)

  def stepForLength(length: Int): Rotation = Rotation(0).set(length, 1)

  def encloses(interval: Interval): Boolean = (calculate(interval.from) <= big) && (calculate(interval.to) >= big)

  def fits(interval: Interval): Boolean = fits(interval.from) && fits(interval.to)

  def fits(arg: Rotation): Boolean = calculate(arg) == big
}
