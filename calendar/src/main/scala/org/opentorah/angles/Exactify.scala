package org.opentorah.angles

import Angles.Rotation

final class Exactify(small: Rotation, mult: Int, round: Int, big: Rotation):

  def findFit: (Interval, Int) = findFit0(round)

  private def findFit0(length: Int): (Interval, Int) =
    val result = find(/*squeeze(*/enclose(length)/*, length)*/, length)
    require(fits(result._1))
    result

  @scala.annotation.tailrec
  private def find(interval: Interval, length: Int): (Interval, Int) =
    if fits(interval) then (interval, length) else find(squeeze(interval, length+1), length+1)

  private def enclose(length: Int): Interval =
    val step: Rotation = stepForLength(length)

    var from: Rotation = small
    var to: Rotation = small

    while (calculate(from) > big) && from.isPositive do from = from-step
    while  calculate(to  ) < big                     do to   = to  +step

    val result = Interval(from, to)

    require(encloses(result))
    result

  private def squeeze(interval: Interval, length: Int): Interval =
    require(encloses(interval))

    val step: Rotation = stepForLength(length)

    var from: Rotation = interval.from
    var to: Rotation = interval.to

    while (from <= to) && (calculate(from) < big) && (calculate(from+step) <= big)                  do from = from+step
    while (from <= to) && (calculate(to  ) > big) && (calculate(to  -step) >= big) && to.isPositive do to   = to  -step

    val result = Interval(from, to)

    require(encloses(result))
    result

  @scala.annotation.tailrec
  def expand(interval: Interval, length: Int, toLength: Int): Interval =
    if length == toLength then interval else expand(expand1(interval, length+1), length+1, toLength)

  private def expand1(interval: Interval, length: Int): Interval =
    require(fits(interval))

    val step: Rotation = stepForLength(length)

    var from: Rotation = interval.from
    var to: Rotation = interval.to

    while calculate(from-step) == big do from = from-step
    while calculate(to  +step) == big do to   = to  +step

    val result = Interval(from, to)

    require(fits(result))
    result

  private def calculate(arg: Rotation): Rotation = (arg*mult).canonical.roundTo(round)

  private def stepForLength(length: Int): Rotation = Rotation(0).set(length, 1)

  private def encloses(interval: Interval): Boolean = (calculate(interval.from) <= big) && (calculate(interval.to) >= big)

  private def fits(interval: Interval): Boolean =
    def fits(arg: Rotation): Boolean = calculate(arg) == big
    fits(interval.from) && fits(interval.to)
