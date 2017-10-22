package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

object MoonSightable {

  // KH 17:3-4
  def forLongitude1(longitude1: Angle, inNortherlyInclinedConstellations: Boolean): Option[Boolean] =
    if (inNortherlyInclinedConstellations) {
      if (longitude1 <= Angle(9)) Some(false)
      else if (longitude1 > Angle(15)) Some(true)
      else None
    } else {
      if (longitude1 <= Angle(10)) Some(false)
      else if (longitude1 > Angle(24)) Some(true)
      else None
    }

  // KH 17:15
  def forArcOfSighting(arcOfSighting: Angle): Option[Boolean] = {
    if (arcOfSighting <= Angle(9)) Some(false) else
    if (arcOfSighting > Angle(14)) Some(true) else
      None
  }

  // KH 17:16-21
  def forSightingLimits(arcOfSighting: Angle, longitude1: Angle): Boolean = {
    // TODO is that what Rambam is saying?
    ((arcOfSighting > Angle( 9)) && (longitude1 >= Angle(13))) || // KH 17:17
    ((arcOfSighting > Angle(10)) && (longitude1 >= Angle(12))) || // KH 17:18
    ((arcOfSighting > Angle(11)) && (longitude1 >= Angle(11))) || // KH 17:19
    ((arcOfSighting > Angle(12)) && (longitude1 >= Angle(10))) || // KH 17:20
    ((arcOfSighting > Angle(13)) && (longitude1 >= Angle( 9)))    // KH 17:21
  }
}
