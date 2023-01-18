package org.opentorah.astronomy

import Angles.Rotation

// KH 15:2-3
object MoonLongitudeDoubleElongationCorrection extends OrderedRotationTable[Rotation](
  " 0°" -> "0°",
  " 6°" -> "1°",
  "12°" -> "2°",
  "19°" -> "3°",
  "25°" -> "4°",
  "32°" -> "5°",
  "39°" -> "6°",
  "46°" -> "7°",
  "52°" -> "8°",
  "60°" -> "9°"
)(Rotation(_)):
  override def calculate(doubleElongation: Rotation): Rotation = find(doubleElongation)
