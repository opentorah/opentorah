package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Position, Rotation}

import scala.math.{abs, asin, cos, pow, round, sin, sqrt}

object MoonAnomalyVisible {
  // As printed
  class Misprinted extends InterpolatedTable[Position] {
    // KH 15:6
    override val values: Map[Position, Rotation] = Map(
      row(  0, 0,  0),
      row( 10, 0, 50),
      row( 20, 1, 38),
      row( 30, 2, 24),
      row( 40, 3,  6),
      row( 50, 3, 44),
      row( 60, 4, 16),
      row( 70, 4, 41),
      row( 80, 5,  0),
      row( 90, 5,  5),
      row(100, 5,  8),
      row(110, 4, 59),
      row(120, 4, 20),
      row(130, 4, 11),
      row(140, 3, 33),
      row(150, 3, 48),
      row(160, 1, 56),
      row(170, 1, 59),
      row(180, 0,  0)
    )

    // KH 15:4, 15:7
    final override def calculate(moonAnomalyTrue: Position): Rotation = {
      val angle: Position = moonAnomalyTrue
      if (angle <= Position(180)) -interpolate(angle) else interpolate(angle.reflect)
    }
  }

  final val misprinted: InterpolatedTable[Position] = new Misprinted

  // Misprints corrected.
  final val table: InterpolatedTable[Position] = new Misprinted {
    override val values: Map[Position, Rotation] = misprinted.values ++ Map(
      row(120, 4, 40),
      row(150, 2, 48),
      row(170, 0, 59)
    )
  }

  def mnasfrome(maslul: Position, e: Double): Rotation = {
    val maslulRadians = maslul.toRadians
    val inRadians = asin(sin(maslulRadians)/sqrt(e*e + 2*e*cos(maslulRadians) + 1))
    Rotation.fromRadians(inRadians, 1)
  }

  def efrommnas(maslul: Position, mnas: Rotation): Double = {
    val maslulRadians = maslul.toRadians
    val mnasRadians = mnas.toRadians
    sin(maslulRadians)/sin(mnasRadians)*abs(cos(mnasRadians))-cos(maslulRadians)
  }

  def efrommnasround(maslul: Position, mnas: Rotation): Double = roundTo(efrommnas(maslul, mnas), 2)

  private def roundTo(value: Double, digits: Int): Double = {
    val quotient = pow(10, digits)
    round(value*quotient)/quotient
  }

  private def row(argumentDegrees: Int, valueDegrees: Int, valueMinutes: Int): (Position, Rotation) =
    Position(argumentDegrees) -> Rotation(valueDegrees, valueMinutes)
}
