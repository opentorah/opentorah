package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Rotation, angleToRadians}

import scala.math.{abs, asin, cos, pow, round, sin, sqrt}

object MoonAnomalyVisible {
  // As printed
  class Misprinted extends InterpolatedTable {
    // KH 15:6
    override val values: Map[Rotation, Rotation] = Map(
      Rotation(  0) -> Rotation(0    ),
      Rotation( 10) -> Rotation(0, 50),
      Rotation( 20) -> Rotation(1, 38),
      Rotation( 30) -> Rotation(2, 24),
      Rotation( 40) -> Rotation(3,  6),
      Rotation( 50) -> Rotation(3, 44),
      Rotation( 60) -> Rotation(4, 16),
      Rotation( 70) -> Rotation(4, 41),
      Rotation( 80) -> Rotation(5,  0),
      Rotation( 90) -> Rotation(5,  5),
      Rotation(100) -> Rotation(5,  8),
      Rotation(110) -> Rotation(4, 59),
      Rotation(120) -> Rotation(4, 20),
      Rotation(130) -> Rotation(4, 11),
      Rotation(140) -> Rotation(3, 33),
      Rotation(150) -> Rotation(3, 48),
      Rotation(160) -> Rotation(1, 56),
      Rotation(170) -> Rotation(1, 59),
      Rotation(180) -> Rotation(0    )
    )

    // KH 15:4, 15:7
    final override def calculate(moonAnomalyTrue: Rotation): Rotation = {
      val angle: Rotation = moonAnomalyTrue.canonical
      if (angle <= Rotation(180)) -interpolate(angle) else interpolate(Rotation(360) - angle)
    }
  }

  final val misprinted: InterpolatedTable = new Misprinted

  // Misprints corrected.
  final val table: InterpolatedTable = new Misprinted {
    override val values: Map[Rotation, Rotation] = misprinted.values
      .updated(Rotation(120), Rotation(4, 40))
      .updated(Rotation(150), Rotation(2, 48))
      .updated(Rotation(170), Rotation(0, 59))
  }

  def mnasfrome(maslul: Rotation, e: Double): Rotation = {
    val inRadians = asin(sin(maslul)/sqrt(e*e + 2*e*cos(maslul) + 1))
    Rotation.fromRadians(inRadians, 1)
  }

  def efrommnas(maslul: Rotation, mnas: Rotation): Double = sin(maslul)/sin(mnas)*abs(cos(mnas))-cos(maslul)

  def efrommnasround(maslul: Rotation, mnas: Rotation): Double = roundTo(efrommnas(maslul, mnas), 2)

  private def roundTo(value: Double, digits: Int): Double = {
    val quotient = pow(10, digits)
    round(value*quotient)/quotient
  }
}
