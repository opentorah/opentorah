package org.opentorah.astronomy

import Angles.Rotation

object MoonSightable:

  // KH 17:3-4
  def forLongitude1(longitude1: Rotation, inNortherlyInclinedConstellations: Boolean): Option[Boolean] =
    if inNortherlyInclinedConstellations then
      if longitude1 <= Rotation("9°") then Some(false)
      else if longitude1 > Rotation("15°") then Some(true)
      else None
    else
      if longitude1 <= Rotation("10°") then Some(false)
      else if longitude1 > Rotation("24°") then Some(true)
      else None

  // KH 17:15
  def forArcOfSighting(arcOfSighting: Rotation): Option[Boolean] =
    if arcOfSighting <= Rotation("9°") then Some(false) else
    if arcOfSighting > Rotation("14°") then Some(true) else
      None

  // KH 17:16-21
  private val limits: List[(Rotation, Rotation)] = List(
    (" 9°", "13°"), // KH 17:17
    ("10°", "12°"), // KH 17:18
    ("11°", "11°"), // KH 17:19
    ("12°", "10°"), // KH 17:20
    ("13°", " 9°")  // KH 17:21
  ).map((arcOfSighting: String, longitude1: String) => (Rotation(arcOfSighting), Rotation(longitude1)))

  def forSightingLimits(arcOfSighting: Rotation, longitude1: Rotation): Boolean = limits
    .exists((limit: (Rotation, Rotation)) => (arcOfSighting > limit._1) && (longitude1 >= limit._2))
