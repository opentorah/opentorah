package org.opentorah.astronomy

import Angles.Rotation

// KH 13:4
object SunLongitudeCorrection extends OrderedRotationTable[Rotation](
  "  0°" -> "0° 0′",
  " 10°" -> "0°20′",
  " 20°" -> "0°40′",
  " 30°" -> "0°58′",
  " 40°" -> "1°15′",
  " 50°" -> "1°29′",
  " 60°" -> "1°41′",
  " 70°" -> "1°51′",
  " 80°" -> "1°57′",
  " 90°" -> "1°59′",
  "100°" -> "1°58′",
  "110°" -> "1°53′",
  "120°" -> "1°45′",
  "130°" -> "1°33′",
  "140°" -> "1°19′",
  "150°" -> "1° 1′",
  "160°" -> "0°42′",
  "170°" -> "0°21′",
  "180°" -> "0° 0′"
)(Rotation(_)):
  // KH 13:2-3
  override def calculate(sunCourse: Rotation): Rotation =
    val angle: Rotation = sunCourse.canonical
    // TODO do the bounding trick like in the MoonAnomalyVisible...
    if angle <= Rotation("180°") then -interpolate(angle) else interpolate((Rotation("360°") - angle).canonical)
