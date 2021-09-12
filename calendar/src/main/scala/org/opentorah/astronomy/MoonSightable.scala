package org.opentorah.astronomy

import org.opentorah.angles.Angles.Rotation

object MoonSightable:

  // KH 17:3-4
  def forLongitude1(longitude1: Rotation, inNortherlyInclinedConstellations: Boolean): Option[Boolean] =
    if inNortherlyInclinedConstellations then
      if longitude1 <= Rotation(9) then Some(false)
      else if longitude1 > Rotation(15) then Some(true)
      else None
    else
      if longitude1 <= Rotation(10) then Some(false)
      else if longitude1 > Rotation(24) then Some(true)
      else None

  // KH 17:15
  def forArcOfSighting(arcOfSighting: Rotation): Option[Boolean] =
    if arcOfSighting <= Rotation(9) then Some(false) else
    if arcOfSighting > Rotation(14) then Some(true) else
      None

  // KH 17:16-21
  def forSightingLimits(arcOfSighting: Rotation, longitude1: Rotation): Boolean =
    ((arcOfSighting > Rotation( 9)) && (longitude1 >= Rotation(13))) || // KH 17:17
    ((arcOfSighting > Rotation(10)) && (longitude1 >= Rotation(12))) || // KH 17:18
    ((arcOfSighting > Rotation(11)) && (longitude1 >= Rotation(11))) || // KH 17:19
    ((arcOfSighting > Rotation(12)) && (longitude1 >= Rotation(10))) || // KH 17:20
    ((arcOfSighting > Rotation(13)) && (longitude1 >= Rotation( 9)))    // KH 17:21
