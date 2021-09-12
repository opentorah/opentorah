package org.opentorah.astronomy

import org.opentorah.angles.Angles
import Angles.{Position, Rotation}
import org.opentorah.calendar.jewish.Jewish.Day
import org.opentorah.numbers.BigRational

final class Calculation(
  val calculator: Calculator,
  val day: Day
):
  // Precision of calculations
  val length: Int = Angles.maxLength

  private def epoch: Epoch = calculator.epoch

  private def calculators: Calculators = calculator.calculators

  private def rounders: Rounders = calculator.rounders

  lazy val daysAfterEpoch: Int = epoch.daysAfterEpoch(day)

  // KH 12:2
  lazy val sunLongitudeMean: Position =
    epoch.sunLongitudeMean + calculators.sunLongitudeMean(daysAfterEpoch)

  // KH 12:2
  lazy val sunApogee: Position = epoch.sunApogee + calculators.sunApogee(daysAfterEpoch)

  // KH 13:1-3,5-6 (maslul; mnas hamaslul)
  def sunCourseRaw: Rotation = sunLongitudeMean - sunApogee
  lazy val sunCourse: Rotation = rounders.sunCourse(sunCourseRaw)
  lazy val sunLongitudeCorrection: Rotation = calculators.sunLongitudeCorrection(sunCourse)
  def sunLongitudeTrueRaw: Position = sunLongitudeMean + sunLongitudeCorrection
  lazy val sunLongitudeTrue: Position = rounders.sunLongitudeTrue(sunLongitudeTrueRaw)

  lazy val moonLongitudeMean: Position =
    epoch.moonLongitudeMean + calculators.moonLongitudeMean(daysAfterEpoch)

  lazy val moonLongitudeAdjustmentForTimeOfSighting: Rotation =
    calculators.moonLongitudeAdjustmentForTimeOfSighting(sunLongitudeMean)

  lazy val moonLongitudeMeanAtTimeOfSighting: Position =
    moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

  // KH 14:4
  lazy val moonAnomalyMean: Position =
    epoch.moonAnomalyMean + calculators.moonAnomalyMean(daysAfterEpoch)

  // KH 15:1-3
  lazy val elongation: Rotation = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
  lazy val doubleElongation: Rotation = elongation * 2

  lazy val moonLongitudeDoubleElongationCorrection: Rotation =
    calculators.moonLongitudeDoubleElongationCorrection(doubleElongation)

  def moonAnomalyTrueRaw: Position = moonAnomalyMean + moonLongitudeDoubleElongationCorrection
  lazy val moonAnomalyTrue: Position = rounders.moonAnomalyTrue(moonAnomalyTrueRaw)

  // KH 15:4
  lazy val moonAnomalyVisible: Rotation =
    rounders.moonAnomalyVisible(calculators.moonAnomalyVisible(moonAnomalyTrue))

  def moonLongitudeTrueRaw: Position = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible
  lazy val moonLongitudeTrue: Position = rounders.moonLongitudeTrue(moonLongitudeTrueRaw)

  // KH 16:3
  def moonHeadMeanReversed: Position = epoch.moonHeadMean + calculators.moonHeadMean(daysAfterEpoch)
  def moonHeadMeanRaw: Position = -moonHeadMeanReversed
  lazy val moonHeadMean: Position = rounders.moonHeadMean(moonHeadMeanRaw)
  def moonTailMeanRaw: Position = moonHeadMeanRaw + Rotation(180)
  def moonTailMean: Position = moonHeadMean + Rotation(180)

  // KH 16:10
  def moonLatitudeCourseRaw: Rotation = moonLongitudeTrue - moonHeadMean
  lazy val moonLatitudeCourse: Rotation = rounders.moonLatitudeCourse(moonLatitudeCourseRaw)
  lazy val isMoonLatitudeNortherly: Boolean = moonLatitudeCourse.canonical < Rotation(180)

  lazy val moonLatitude: Rotation = calculators.moonLatitude(moonLatitudeCourse)

  // KH 17:1
  lazy val longitude1: Rotation = rounders.longitude1(moonLongitudeTrue - sunLongitudeTrue)
  // KH 17:2
  lazy val latitude1: Rotation = moonLatitude

  // KH 17:3-4
  lazy val inNortherlyInclinedConstellations: Boolean = Zodiac.in(moonLongitudeTrue, Set(
    Zodiac.Capricorn, Zodiac.Aquarius, Zodiac.Pisces, Zodiac.Aries, Zodiac.Taurus, Zodiac.Gemini
  ))

  // KH 17:5-6
  lazy val longitudeSightingAdjustment: Rotation =
    calculators.moonLongitudeSightingAdjustment(moonLongitudeTrue)
  lazy val longitude2: Rotation = rounders.longitude2(longitude1 - longitudeSightingAdjustment)

  // KH 17:7-9
  lazy val latitudeSightingAdjustment: Rotation =
    calculators.moonLatitudeSightingAdjustment(moonLongitudeTrue)
  lazy val latitude2: Rotation =
    if isMoonLatitudeNortherly then latitude1 - latitudeSightingAdjustment
    else latitude1 + latitudeSightingAdjustment

  // KH 17:10
  lazy val moonCircuitPortion: BigRational = calculators.moonCircuitPortion(moonLongitudeTrue)
  lazy val moonCircuit: Rotation = rounders.moonCircuit(latitude2 *(moonCircuitPortion, length))

  // KH 17:11
  lazy val longitude3: Rotation =
    rounders.longitude3(if
      (isMoonLatitudeNortherly && inNortherlyInclinedConstellations) ||
      (!isMoonLatitudeNortherly && !inNortherlyInclinedConstellations)
    then longitude2 - moonCircuit else longitude2 + moonCircuit)

  // KH 17:12
  lazy val moonLongitude3Portion: BigRational =
    calculators.moonLongitude3Portion(moonLongitudeTrue)
  lazy val moonLongitude3Correction: Rotation =
    rounders.moonLongitude3Correction(longitude3 *(moonLongitude3Portion, length))
  lazy val longitude4: Rotation = longitude3 + moonLongitude3Correction

  // KH 17:12
  lazy val geographicCorrection: Rotation =
    rounders.geographicCorrection(latitude1 *(BigRational(2, 3), length))
  lazy val arcOfSighting: Rotation = rounders.arcOfSighting(
    if isMoonLatitudeNortherly then longitude4 + geographicCorrection
    else longitude4 - geographicCorrection)

  // KH 17:3-4,15-21
  lazy val isMoonSightable: Boolean =
    MoonSightable.forLongitude1(longitude1, inNortherlyInclinedConstellations)
      .orElse(MoonSightable.forArcOfSighting(arcOfSighting))
      .getOrElse(MoonSightable.forSightingLimits(arcOfSighting, longitude1))
