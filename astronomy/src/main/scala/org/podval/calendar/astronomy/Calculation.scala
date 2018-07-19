package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.numbers.BigRational

final class Calculation(
  val calculator: Calculator,
  val day: Day
) {
  private def epoch: Epoch = calculator.epoch

  private def calculators: Calculators = calculator.calculators

  private def rounders: Rounders = calculator.rounders

  lazy val daysAfterEpoch: Int = epoch.daysAfterEpoch(day)

  // KH 12:2
  lazy val sunLongitudeMean: AnglePoint =
    epoch.sunLongitudeMean + calculators.sunLongitudeMean(daysAfterEpoch)

  // KH 12:2
  // TODO Rambam says "the same way", but doesn't give value for 1 day...
  lazy val sunApogee: AnglePoint = epoch.sunApogee + calculators.sunApogee(daysAfterEpoch)

  // KH 13:1-3,5-6 (maslul; mnas hamaslul)
  def sunCourseRaw: Angle = sunLongitudeMean - sunApogee
  lazy val sunCourse: Angle = rounders.sunCourse(sunCourseRaw)
  lazy val sunLongitudeCorrection: Angle = calculators.sunLongitudeCorrection(sunCourse)
  def sunLongitudeTrueRaw: AnglePoint = sunLongitudeMean + sunLongitudeCorrection
  // TODO calculate for a moment, not just day.
  lazy val sunLongitudeTrue: AnglePoint = rounders.sunLongitudeTrue(sunLongitudeTrueRaw)

  // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
  // but no algorithm is given.

  lazy val moonLongitudeMean: AnglePoint =
    epoch.moonLongitudeMean + calculators.moonLongitudeMean(daysAfterEpoch)

  lazy val moonLongitudeAdjustmentForTimeOfSighting: Angle =
    calculators.moonLongitudeAdjustmentForTimeOfSighting(sunLongitudeMean)

  lazy val moonLongitudeMeanAtTimeOfSighting: AnglePoint =
    moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

  // KH 14:4
  lazy val moonAnomalyMean: AnglePoint =
    epoch.moonAnomalyMean + calculators.moonAnomalyMean(daysAfterEpoch)

  // KH 15:1-3
  // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
  lazy val elongation: Angle = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
  lazy val doubleElongation: Angle = elongation * 2

  lazy val moonLongitudeDoubleElongationCorrection: Angle =
    calculators.moonLongitudeDoubleElongationCorrection(doubleElongation)

  def moonAnomalyTrueRaw: AnglePoint = moonAnomalyMean + moonLongitudeDoubleElongationCorrection
  lazy val moonAnomalyTrue: AnglePoint = rounders.moonAnomalyTrue(moonAnomalyTrueRaw)

  // KH 15:4
  lazy val moonAnomalyVisible: Angle =
    rounders.moonAnomalyVisible(calculators.moonAnomalyVisible(moonAnomalyTrue.toVector))

  def moonLongitudeTrueRaw: AnglePoint = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible
  lazy val moonLongitudeTrue: AnglePoint = rounders.moonLongitudeTrue(moonLongitudeTrueRaw)

  // KH 16:3
  def moonHeadMeanReversed: AnglePoint = epoch.moonHeadMean + calculators.moonHeadMean(daysAfterEpoch)
  def moonHeadMeanRaw: AnglePoint = -moonHeadMeanReversed
  lazy val moonHeadMean: AnglePoint = rounders.moonHeadMean(moonHeadMeanRaw)
  def moonTailMeanRaw: AnglePoint = moonHeadMeanRaw + Angle(180)
  def moonTailMean: AnglePoint = moonHeadMean + Angle(180)

  // KH 16:10
  def moonLatitudeCourseRaw: Angle = (moonLongitudeTrue - moonHeadMean).canonical
  lazy val moonLatitudeCourse: Angle = rounders.moonLatitudeCourse(moonLatitudeCourseRaw)
  lazy val isMoonLatitudeNortherly: Boolean = moonLatitudeCourse < Angle(180)
  lazy val moonLatitude: Angle = calculators.moonLatitude(moonLatitudeCourse) // TODO AnglePoint

  // KH 17:1
  lazy val longitude1: Angle = rounders.longitude1(moonLongitudeTrue - sunLongitudeTrue) // TODO AnglePoint
  // KH 17:2
  lazy val latitude1: Angle = moonLatitude // TODO AnglePoint

  // KH 17:3-4
  lazy val inNortherlyInclinedConstellations: Boolean = Zodiac.in(moonLongitudeTrue, Set(
    Zodiac.Capricorn, Zodiac.Aquarius, Zodiac.Pisces, Zodiac.Aries, Zodiac.Taurus, Zodiac.Gemini
  ))

  // KH 17:5-6
  lazy val longitudeSightingAdjustment: Angle =
    calculators.moonLongitudeSightingAdjustment(moonLongitudeTrue)
  lazy val longitude2: Angle = rounders.longitude2(longitude1 - longitudeSightingAdjustment) // TODO AnglePoint

  // KH 17:7-9
  lazy val latitudeSightingAdjustment: Angle =
    calculators.moonLatitudeSightingAdjustment(moonLongitudeTrue)
  lazy val latitude2: Angle = // TODO AnglePoint
    if (isMoonLatitudeNortherly) latitude1 - latitudeSightingAdjustment
    else latitude1 + latitudeSightingAdjustment

  // KH 17:10
  lazy val moonCircuitPortion: BigRational = calculators.moonCircuitPortion(moonLongitudeTrue)
  lazy val moonCircuit: Angle = rounders.moonCircuit(latitude2 * moonCircuitPortion)

  // KH 17:11
  lazy val longitude3: Angle = // TODO AnglePoint
    rounders.longitude3(if (
      (isMoonLatitudeNortherly && inNortherlyInclinedConstellations) ||
      (!isMoonLatitudeNortherly && !inNortherlyInclinedConstellations)
    ) longitude2 - moonCircuit else longitude2 + moonCircuit)

  // KH 17:12
  lazy val moonLongitude3Portion: BigRational =
    calculators.moonLongitude3Portion(moonLongitudeTrue)
  /* TODO longitude3.toPoint?*/
  lazy val moonLongitude3Correction: Angle =
    rounders.moonLongitude3Correction(longitude3 * moonLongitude3Portion)
  lazy val longitude4: Angle = longitude3 + moonLongitude3Correction // TODO AnglePoint

  // KH 17:12
  lazy val geographicCorrection: Angle =
    rounders.geographicCorrection(latitude1 * BigRational(2, 3))
  lazy val arcOfSighting: Angle = rounders.arcOfSighting(
    if (isMoonLatitudeNortherly) longitude4 + geographicCorrection
    else longitude4 - geographicCorrection)

  // KH 17:3-4,15-21
  lazy val isMoonSightable: Boolean =
    MoonSightable.forLongitude1(longitude1, inNortherlyInclinedConstellations)
      .orElse(MoonSightable.forArcOfSighting(arcOfSighting))
      .getOrElse(MoonSightable.forSightingLimits(arcOfSighting, longitude1))

  // TODO crescent calculations: KH 18-19!
}
