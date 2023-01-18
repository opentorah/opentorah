package org.opentorah.astronomy

import Angles.{Position, Rotation}

import scala.math.{abs, asin, cos, pow, round, sin, sqrt}

object MoonAnomalyVisible:
  // KH 15:6
  private val dataMisprinted: Map[String, String] = Map(
    "  0°" -> "0° 0′",
    " 10°" -> "0°50′",
    " 20°" -> "1°38′",
    " 30°" -> "2°24′",
    " 40°" -> "3° 6′",
    " 50°" -> "3°44′",
    " 60°" -> "4°16′",
    " 70°" -> "4°41′",
    " 80°" -> "5° 0′",
    " 90°" -> "5° 5′",
    "100°" -> "5° 8′",
    "110°" -> "4°59′",
    "120°" -> "4°20′",
    "130°" -> "4°11′",
    "140°" -> "3°33′",
    "150°" -> "3°48′",
    "160°" -> "1°56′",
    "170°" -> "1°59′",
    "180°" -> "0° 0′"
  )

  private val dataCorrected: Map[String, String] = dataMisprinted ++ Map(
    "120°" -> "4°40′",
    "150°" -> "2°48′",
    "170°" -> "0°59′"
  )

  class Table(values: Map[String, String]) extends OrderedRotationTable[Position](values.toList*)(Position(_)):

    // KH 15:4, 15:7
    final override def calculate(moonAnomalyTrue: Position): Rotation =
      val angle: Position = moonAnomalyTrue
      if angle <= Position("180°") then -interpolate(angle) else interpolate(reflect(angle))

  object misprinted extends Table(dataMisprinted)
  object corrected  extends Table(dataCorrected)

  private final def reflect(position: Position): Position =
    Position.zero + (Angles.period - (position - Position.zero))

  def mnasfrome(maslul: Position, e: Double): Rotation =
    val maslulRadians = maslul.toRadians
    val inRadians = asin(sin(maslulRadians)/sqrt(e*e + 2*e*cos(maslulRadians) + 1))
    Rotation.fromRadians(inRadians, 1)

  def efrommnas(maslul: Position, mnas: Rotation): Double =
    val maslulRadians = maslul.toRadians
    val mnasRadians = mnas.toRadians
    sin(maslulRadians)/sin(mnasRadians)*abs(cos(mnasRadians))-cos(maslulRadians)

  def efrommnasround(maslul: Position, mnas: Rotation): Double = roundTo(efrommnas(maslul, mnas), 2)

  private def roundTo(value: Double, digits: Int): Double =
    val quotient = pow(10, digits)
    round(value*quotient)/quotient
