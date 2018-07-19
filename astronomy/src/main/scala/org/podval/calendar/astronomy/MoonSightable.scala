package org.podval.calendar.astronomy

import org.podval.calendar.angle.Angles.Rotation

object MoonSightable {

  // KH 17:3-4
  def forLongitude1(longitude1: Rotation, inNortherlyInclinedConstellations: Boolean): Option[Boolean] =
    if (inNortherlyInclinedConstellations) {
      if (longitude1 <= Rotation(9)) Some(false)
      else if (longitude1 > Rotation(15)) Some(true)
      else None
    } else {
      if (longitude1 <= Rotation(10)) Some(false)
      else if (longitude1 > Rotation(24)) Some(true)
      else None
    }

  // KH 17:15
  def forArcOfSighting(arcOfSighting: Rotation): Option[Boolean] = {
    if (arcOfSighting <= Rotation(9)) Some(false) else
    if (arcOfSighting > Rotation(14)) Some(true) else
      None
  }

  // KH 17:16-21
  // TODO add a calculator method for this!
  def forSightingLimits(arcOfSighting: Rotation, longitude1: Rotation): Boolean = {
    // TODO is that what Rambam is saying?
    ((arcOfSighting > Rotation( 9)) && (longitude1 >= Rotation(13))) || // KH 17:17
    ((arcOfSighting > Rotation(10)) && (longitude1 >= Rotation(12))) || // KH 17:18
    ((arcOfSighting > Rotation(11)) && (longitude1 >= Rotation(11))) || // KH 17:19
    ((arcOfSighting > Rotation(12)) && (longitude1 >= Rotation(10))) || // KH 17:20
    ((arcOfSighting > Rotation(13)) && (longitude1 >= Rotation( 9)))    // KH 17:21
  }
}
