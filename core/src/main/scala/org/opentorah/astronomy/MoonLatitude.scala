package org.opentorah.astronomy

import Angles.Rotation

// KH 16:11
object MoonLatitude extends OrderedRotationTable[Rotation](
  " 0°" -> "0° 0′",
  "10°" -> "0°52′",
  "20°" -> "1°43′",
  "30°" -> "2°30′",
  "40°" -> "3°13′",
  "50°" -> "3°50′",
  "60°" -> "4°20′",
  "70°" -> "4°42′",
  "80°" -> "4°55′",
  "90°" -> "5° 0′"
)(Rotation(_)):
  // KH 16:13-15
  override def calculate(moonLatitudeCourse: Rotation): Rotation =
    val angle: Rotation = moonLatitudeCourse.canonical
    // canonical angle is always >= 0°
    val argument: Rotation =
      if angle <= Rotation(" 90°") then angle                    else // KH 16:11
      if angle <= Rotation("180°") then Rotation("180°") - angle else // KH 16:13
      if angle <= Rotation("270°") then angle - Rotation("180°") else // KH 16:14
                                        Rotation("360°") - angle      // KH 16:15

    interpolate(argument.canonical)
