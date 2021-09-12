package org.opentorah.angles

import Angles.Rotation

final class Interval(val from: Rotation, val to: Rotation):
  require(from <= to)

  override def toString: String = s"[$from..$to]"

  def contains(value: Rotation): Boolean = (from <= value) && (value <= to)

  def intersect(that: Interval): Interval =
    val result = Interval(
      from = if this.from < that.from then that.from else this.from,
      to =   if this.to   > that.to   then that.to   else this.to
    )
    println(s"$this intersect $that = $result")
    result
