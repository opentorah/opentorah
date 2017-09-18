package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, angleToRadians}
import Angle2Angle.Table
import scala.math.{abs, asin, cos, pow, round, sin, sqrt}

// KH 15:6
object MoonAnomalyVisible extends Angle2Angle {
  // As printed
  class Misprinted extends Table {
    override val a10 : Angle = Angle(0, 50)
    override val a20 : Angle = Angle(1, 38)
    override val a30 : Angle = Angle(2, 24)
    override val a40 : Angle = Angle(3,  6)
    override val a50 : Angle = Angle(3, 44)
    override val a60 : Angle = Angle(4, 16)
    override val a70 : Angle = Angle(4, 41)
    override val a80 : Angle = Angle(5,  0)
    override val a90 : Angle = Angle(5,  5)
    override val a100: Angle = Angle(5,  8)
    override val a110: Angle = Angle(4, 59)
    override val a120: Angle = Angle(4, 20)
    override val a130: Angle = Angle(4, 11)
    override val a140: Angle = Angle(3, 33)
    override val a150: Angle = Angle(3, 48)
    override val a160: Angle = Angle(1, 56)
    override val a170: Angle = Angle(1, 59)
  }

  final val misprinted: Table = new Misprinted

  // Misprints corrected.
  final override val table: Table = new Misprinted {
    override val a120: Angle = Angle(4, 40)
    override val a150: Angle = Angle(2, 48)
    override val a170: Angle = Angle(0, 59)
  }

  def mnasfrome(maslul: Angle, e: Double): Angle = {
    val inRadians = asin(sin(maslul)/sqrt(e*e + 2*e*cos(maslul) + 1))
    Angle.fromRadians(inRadians, 1)
  }

  def efrommnas(maslul: Angle, mnas: Angle): Double = sin(maslul)/sin(mnas)*abs(cos(mnas))-cos(maslul)

  def efrommnasround(maslul: Angle, mnas: Angle): Double = roundTo(efrommnas(maslul, mnas), 2)

  private def roundTo(value: Double, digits: Int): Double = {
    val quotient = pow(10, digits)
    round(value*quotient)/quotient
  }
}
