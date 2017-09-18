package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.jewish.Jewish.Day

object Sun {

  // KH 12:2
  // (according to note 12 in Moznaim Rambam in English: at 6PM)
  val longitudeMeanAtEpoch: AnglePoint = Zodiac.Aries.at(Angle(7, 3, 32))

  // KH 12:2
  final def longitudeMean(day: Day): AnglePoint =
    longitudeMeanAtEpoch + SunLongitudeMean.fromTable(day.number-Epoch.epoch.number)

  // KH 12:2
  val apogeeAtEpoch: AnglePoint = Zodiac.Gemini.at(Angle(26, 45, 8))

  // KH 12:2
  // TODO Rambam says "the same way", but doesn't give value for 1 day...
  final def apogee(day: Day): AnglePoint =
    apogeeAtEpoch + SunApogee.fromTable(day.number-Epoch.epoch.number)

  // KH 13:1-3,5-6 (maslul; mnas hamaslul)
  final def longitudeTrue(day: Day): AnglePoint = {
    val longitude = longitudeMean(day)
    val correction = SunLongitudeCorrection.fromTable(longitude - apogee(day))
    longitude + correction
  }

  // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
  // but no algotithm is given.
}
