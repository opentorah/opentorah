package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint, defaultLength}
import org.podval.calendar.numbers.BigRational

// TODO virtualize calls to roundTo()

trait Calculator {
  def epoch: Day

  def sunLongitudeMeanAtEpoch: AnglePoint

  def sunLongitudeMeanCalculator: Int => Angle

  def sunApogeeAtEpoch: AnglePoint

  def sunApogeeCalculator: Int => Angle

  def sunLongitudeCorrectionCalculator: Angle => Angle

  def moonLongitudeMeanAtEpoch: AnglePoint

  def moonLongitudeMeanCalculator: Int => Angle

  def moonLongitudeAdjustmentForTimeOfSightingCalculator: AnglePoint => Angle

  def moonAnomalyMeanAtEpoch: AnglePoint

  def moonAnomalyMeanCalculator: Int => Angle

  def moonLongitudeDoubleElongationCorrectionCalculator: Angle => Angle

  def moonAnomalyVisibleCalculator: Angle => Angle

  def moonHeadMeanAtEpoch: AnglePoint

  def moonHeadMeanCalculator: Int => Angle

  def moonLatitudeCalculator: Angle => Angle

  def moonLongitudeSightingAdjustmentCalculator: AnglePoint => Angle

  def moonLatitudeSightingAdjustmentCalculator: AnglePoint => Angle

  def moonCircuitPortionCalculator: AnglePoint => BigRational

  def moonLongitude3PortionCalculator: AnglePoint => BigRational

  final def calculate(day: Day): Calculator.Result = {
    val daysAfterEpoch: Int = day.number - epoch.number

    // KH 12:2
    val sunLongitudeMean: AnglePoint =
      sunLongitudeMeanAtEpoch + sunLongitudeMeanCalculator(daysAfterEpoch)

    // KH 12:2
    // TODO Rambam says "the same way", but doesn't give value for 1 day...
    val sunApogee: AnglePoint =
      sunApogeeAtEpoch + sunApogeeCalculator(daysAfterEpoch)

    // KH 13:1-3,5-6 (maslul; mnas hamaslul)
    val sunCourse: Angle = sunLongitudeMean - sunApogee
    val sunLongitudeCorrection: Angle = sunLongitudeCorrectionCalculator(sunCourse)
    val sunLongitudeTrue: AnglePoint = sunLongitudeMean + sunLongitudeCorrection

    // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
    // but no algorithm is given.

    val moonLongitudeMean: AnglePoint =
      moonLongitudeMeanAtEpoch + moonLongitudeMeanCalculator(daysAfterEpoch)

    val moonLongitudeAdjustmentForTimeOfSighting: Angle =
      moonLongitudeAdjustmentForTimeOfSightingCalculator(sunLongitudeMean)

    val moonLongitudeMeanAtTimeOfSighting: AnglePoint =
      moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

    // KH 14:4
    val moonAnomalyMean: AnglePoint =
      moonAnomalyMeanAtEpoch + moonAnomalyMeanCalculator(daysAfterEpoch)

    // KH 15:1-3
    // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
    val elongation: Angle = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
    val doubleElongation: Angle = elongation*2

    val moonLongitudeDoubleElongationCorrection: Angle =
      moonLongitudeDoubleElongationCorrectionCalculator(doubleElongation)

    val moonAnomalyTrue: AnglePoint = moonAnomalyMean + moonLongitudeDoubleElongationCorrection

    // KH 15:4
    val moonAnomalyVisible: Angle =
      moonAnomalyVisibleCalculator(moonAnomalyTrue.toInterval).roundToMinutes

    val moonLongitudeTrue: AnglePoint = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible

    // KH 16:3
    val moonHeadMeanReversed: AnglePoint = moonHeadMeanAtEpoch + moonHeadMeanCalculator(daysAfterEpoch)
    val moonHeadMean: AnglePoint = -moonHeadMeanReversed
    val moonTailMean: AnglePoint = moonHeadMean + Angle(180)

    // KH 16:10
    val moonLatitudeCourse: Angle =
      (moonLongitudeTrue.roundToMinutes - moonHeadMean.roundToMinutes).canonical
    val isMoonLatitudeNortherly: Boolean = moonLatitudeCourse < Angle(180)
    val moonLatitude: Angle = moonLatitudeCalculator(moonLatitudeCourse) // TODO AnglePoint

    // KH 17:1
    val longitude1: Angle = moonLongitudeTrue - sunLongitudeTrue // TODO AnglePoint
    // KH 17:2
    val latitude1: Angle = moonLatitude // TODO AnglePoint

    // KH 17:3-4
    val inNortherlyInclinedConstellations: Boolean = Zodiac.in(moonLongitudeTrue, Set(
      Zodiac.Capricorn, Zodiac.Aquarius, Zodiac.Pisces, Zodiac.Aries, Zodiac.Taurus, Zodiac.Gemini
    ))

    val result1: Option[Boolean] =
      if (inNortherlyInclinedConstellations) {
        if (longitude1 <= Angle(9)) Some(false)
        else if (longitude1 > Angle(15)) Some(true)
        else None
      } else {
        if (longitude1 <= Angle(10)) Some(false)
        else if (longitude1 > Angle(24)) Some(true)
        else None
      }

    // KH 17:5-6
    val longitudeSightingAdjustment: Angle =
      moonLongitudeSightingAdjustmentCalculator(moonLongitudeTrue)
    val longitude2: Angle = longitude1 - longitudeSightingAdjustment // TODO AnglePoint

    // KH 17:7-9
    val latitudeSightingAdjustment: Angle =
      moonLatitudeSightingAdjustmentCalculator(moonLongitudeTrue)
    val latitude2: Angle =  // TODO AnglePoint
      if (isMoonLatitudeNortherly) latitude1 - latitudeSightingAdjustment
      else latitude1 + latitudeSightingAdjustment

    // KH 17:10
    val moonCircuitPortion: BigRational = moonCircuitPortionCalculator(moonLongitudeTrue)
    val moonCircuit: Angle = latitude2*(moonCircuitPortion, defaultLength)

    // KH 17:11
    val longitude3: Angle = // TODO AnglePoint
      if (
        (isMoonLatitudeNortherly && inNortherlyInclinedConstellations) ||
        (!isMoonLatitudeNortherly && !inNortherlyInclinedConstellations)
      ) longitude2 - moonCircuit else longitude2 + moonCircuit

    // KH 17:12
    val moonLongitude3Portion: BigRational = moonLongitude3PortionCalculator(moonLongitudeTrue/* TODO longitude3.toPoint?*/)
    val moonLongitude3Correction: Angle = longitude3*(moonLongitude3Portion, defaultLength)
    val longitude4: Angle = longitude3 + moonLongitude3Correction // TODO AnglePoint

    // KH 17:12
    val geographicCorrection: Angle = latitude1*(BigRational(2, 3), defaultLength)
    val arcOfSighting: Angle =
      if (isMoonLatitudeNortherly) longitude4 + geographicCorrection
      else longitude4 - geographicCorrection

    val isMoonSightable: Boolean = result1.getOrElse {
      // KH 17:15
      if (arcOfSighting <= Angle(9)) false else
      if (arcOfSighting > Angle(14)) true else {
        // KH 17:16-21
        true // TODO can't understand what Rambam is saying: "from 9 to 10 or more than 10"?!
      }
    }

    Calculator.Result(
      day,
      daysAfterEpoch,
      sunLongitudeMean,
      sunApogee,
      sunCourse,
      sunLongitudeCorrection,
      sunLongitudeTrue,
      moonLongitudeMean,
      moonLongitudeAdjustmentForTimeOfSighting,
      moonLongitudeMeanAtTimeOfSighting,
      moonAnomalyMean,
      elongation,
      doubleElongation,
      moonLongitudeDoubleElongationCorrection,
      moonAnomalyTrue,
      moonAnomalyVisible,
      moonLongitudeTrue,
      moonHeadMeanReversed,
      moonHeadMean,
      moonTailMean,
      moonLatitudeCourse,
      moonLatitude,
      isMoonLatitudeNortherly,
      longitude1,
      latitude1,
      longitudeSightingAdjustment,
      latitudeSightingAdjustment,
      longitude2,
      latitude2,
      moonCircuitPortion,
      moonCircuit,
      longitude3,
      moonLongitude3Portion,
      moonLongitude3Correction,
      longitude4,
      geographicCorrection,
      arcOfSighting,
      isMoonSightable
    )
  }
}


object Calculator {
  case class Result(
    day: Day,
    daysAfterEpoch: Int,
    sunLongitudeMean: AnglePoint,
    sunApogee: AnglePoint,
    sunCourse: Angle,
    sunLongitudeCorrection: Angle,
    sunLongitudeTrue: AnglePoint,
    moonLongitudeMean: AnglePoint,
    moonLongitudeAdjustmentForTimeOfSighting: Angle,
    moonLongitudeMeanAtTimeOfSighting: AnglePoint,
    moonAnomalyMean: AnglePoint,
    elongation: Angle,
    doubleElongation: Angle,
    moonLongitudeDoubleElongationCorrection: Angle,
    moonAnomalyTrue: AnglePoint,
    moonAnomalyVisible: Angle,
    moonLongitudeTrue: AnglePoint,
    moonHeadMeanReversed: AnglePoint,
    moonHeadMean: AnglePoint,
    moonTailMean: AnglePoint,
    moonLatitudeCourse: Angle,
    moonLatitude: Angle,
    isMoonLatitudeNortherly: Boolean,
    longitude1: Angle,
    latitude1: Angle,
    longitudeSightingAdjustment: Angle,
    latitudeSightingAdjustment: Angle,
    longitude2: Angle,
    latitude2: Angle,
    moonCircuitPortion: BigRational,
    moonCircuit: Angle,
    longitude3: Angle,
    moonLongitude3Portion: BigRational,
    moonLongitude3Correction: Angle,
    longitude4: Angle,
    geographicCorrection: Angle,
    arcOfSighting: Angle,
    isMoonSightable: Boolean
  )

  object TableCalculator extends Calculator {
    final override def epoch: Day = Epoch.epoch
    final override def sunLongitudeMeanAtEpoch: AnglePoint = SunLongitudeMean.atEpoch
    final override def sunLongitudeMeanCalculator: Int => Angle = SunLongitudeMean.table.calculate
    final override def sunApogeeAtEpoch: AnglePoint = SunApogee.atEpoch
    final override def sunApogeeCalculator: Int => Angle = SunApogee.table.calculate
    final override def sunLongitudeCorrectionCalculator: Angle => Angle =
      SunLongitudeCorrection.table.calculate
    final override def moonLongitudeMeanAtEpoch: AnglePoint = MoonLongitudeMean.atEpoch
    final override def moonLongitudeMeanCalculator: Int => Angle =
      MoonLongitudeMean.table.calculate
    final override def moonLongitudeAdjustmentForTimeOfSightingCalculator: AnglePoint => Angle =
      MoonLongitudeAdjustmentForTimeOfSighting.calculate
    final override def moonAnomalyMeanAtEpoch: AnglePoint = MoonAnomalyMean.atEpoch
    final override def moonAnomalyMeanCalculator: Int => Angle = MoonAnomalyMean.table.calculate
    final def moonLongitudeDoubleElongationCorrectionCalculator: Angle => Angle =
      MoonLongitudeDoubleElongationCorrection.calculate
    final override def moonAnomalyVisibleCalculator: Angle => Angle =
      MoonAnomalyVisible.table.calculate
    final override def moonHeadMeanAtEpoch: AnglePoint = MoonHeadMean.atEpoch
    final override def moonHeadMeanCalculator: Int => Angle = MoonHeadMean.table.calculate
    final override def moonLatitudeCalculator: Angle => Angle = MoonLatitude.table.calculate
    final override def moonLongitudeSightingAdjustmentCalculator: AnglePoint => Angle =
      MoonLongitudeSightingAdjustment.calculate
    final override def moonLatitudeSightingAdjustmentCalculator: AnglePoint => Angle =
      MoonLatitudeSightingAdjustment.calculate
    final override def moonCircuitPortionCalculator: AnglePoint => BigRational =
      MoonCircuitPortion.calculate
    final override def moonLongitude3PortionCalculator: AnglePoint => BigRational =
      MoonLongitude3Portion.calculate
  }
}
