package org.podval.calendar.angles

import org.podval.calendar.angles.Angles.Rotation

final case class Interval(from: Rotation, to: Rotation) {
  require(from <= to)

  override def toString: String = s"[$from..$to]"

  def contains(value: Rotation): Boolean = (from <= value) && (value <= to)

  def intersect(that: Interval): Interval = {
    val result = Interval(
      from = if (this.from < that.from) that.from else this.from,
      to = if (this.to > that.to) that.to else this.to
    )
    println(s"$this intersect $that = $result")
    result
  }
}
