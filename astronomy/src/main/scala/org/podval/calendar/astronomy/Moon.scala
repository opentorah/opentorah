package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.jewish.Jewish.Day

object Moon {

  // KH 14:4
  // (according to note 12 in Moznaim Rambam in English: at 6PM)
  val longitudeMeanAtEpoch: AnglePoint = Zodiac.Taurus.at(Angle(1, 14, 43))

  final def longitudeMean(day: Day): AnglePoint =
    longitudeMeanAtEpoch + MoonLongitudeMean.fromTable(day.number-Epoch.epoch.number)

  final def longitudeMeanAtTimeOfSighting(day: Day): AnglePoint =
    longitudeMeanAtTimeOfSighting(day, Sun.longitudeMean(day))

  final def longitudeMeanAtTimeOfSighting(day: Day, sunLongitudeMean: AnglePoint): AnglePoint =
    longitudeMean(day) + MoonLongitudeAdjustmentForTimeOfSighting.adjustment(sunLongitudeMean)

  // KH 14:4
  val anomalyAtEpoch: AnglePoint = AnglePoint(84, 28, 42)

  // KH 14:4
  final def anomalyMean(day: Day): AnglePoint =
    anomalyAtEpoch + MoonAnomalyMean.fromTable(day.number-Epoch.epoch.number)

  // KH 15:1-3
  // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
  final def anomalyTrue(day: Day, moonLongitudeMean: AnglePoint, sunLongitudeMean: AnglePoint): AnglePoint = {
    val elongation: Angle = moonLongitudeMean - sunLongitudeMean
    val correction: Angle =
      MoonLongitudeDoubleElongationCorrection.correction(elongation*2)
    anomalyMean(day) + correction
  }

  // KH 15:4
  final def longitudeTrueAtTimeOfSighting(day: Day): AnglePoint = {
    val sunLongitudeMean: AnglePoint = Sun.longitudeMean(day)
    val meanAtTimeOfSighting: AnglePoint = longitudeMeanAtTimeOfSighting(day, sunLongitudeMean)
    val anomaly: AnglePoint = anomalyTrue(day, meanAtTimeOfSighting, sunLongitudeMean)
    val anomalyVisible: Angle = MoonAnomalyVisible.fromTable(anomaly.toInterval)
    meanAtTimeOfSighting + anomalyVisible
  }
}
