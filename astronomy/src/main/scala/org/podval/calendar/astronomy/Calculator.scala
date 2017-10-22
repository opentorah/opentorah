package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint, defaultLength}
import org.podval.calendar.numbers.BigRational

class Calculator(epoch: Epoch, calculators: Calculators, rounders: Rounders) {
  def calculate(day: Day): Calculator.Result = {
    val daysAfterEpoch: Int = epoch.daysAfterEpoch(day)

    // KH 12:2
    val sunLongitudeMean: AnglePoint =
      epoch.sunLongitudeMean + calculators.sunLongitudeMean(daysAfterEpoch)

    // KH 12:2
    // TODO Rambam says "the same way", but doesn't give value for 1 day...
    val sunApogee: AnglePoint = epoch.sunApogee + calculators.sunApogee(daysAfterEpoch)

    // KH 13:1-3,5-6 (maslul; mnas hamaslul)
    val sunCourseRaw: Angle = sunLongitudeMean - sunApogee
    val sunCourse: Angle = rounders.sunCourse(sunCourseRaw)
    val sunLongitudeCorrection: Angle = calculators.sunLongitudeCorrection(sunCourse)
    val sunLongitudeTrueRaw: AnglePoint = sunLongitudeMean + sunLongitudeCorrection
    val sunLongitudeTrue: AnglePoint = rounders.sunLongitudeTrue(sunLongitudeTrueRaw)

    // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
    // but no algorithm is given.

    val moonLongitudeMean: AnglePoint =
      epoch.moonLongitudeMean + calculators.moonLongitudeMean(daysAfterEpoch)

    val moonLongitudeAdjustmentForTimeOfSighting: Angle =
      calculators.moonLongitudeAdjustmentForTimeOfSighting(sunLongitudeMean)

    val moonLongitudeMeanAtTimeOfSighting: AnglePoint =
      moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

    // KH 14:4
    val moonAnomalyMean: AnglePoint =
      epoch.moonAnomalyMean + calculators.moonAnomalyMean(daysAfterEpoch)

    // KH 15:1-3
    // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
    val elongation: Angle = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
    val doubleElongation: Angle = elongation*2

    val moonLongitudeDoubleElongationCorrection: Angle =
      calculators.moonLongitudeDoubleElongationCorrection(doubleElongation)

    val moonAnomalyTrueRaw: AnglePoint = moonAnomalyMean + moonLongitudeDoubleElongationCorrection
    val moonAnomalyTrue: AnglePoint = rounders.moonAnomalyTrue(moonAnomalyTrueRaw)

    // KH 15:4
    val moonAnomalyVisible: Angle =
      rounders.moonAnomalyVisible(calculators.moonAnomalyVisible(moonAnomalyTrue.toInterval))

    val moonLongitudeTrueRaw: AnglePoint = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible
    val moonLongitudeTrue: AnglePoint = rounders.moonLongitudeTrue(moonLongitudeTrueRaw)

    // KH 16:3
    val moonHeadMeanReversed: AnglePoint = epoch.moonHeadMean + calculators.moonHeadMean(daysAfterEpoch)
    val moonHeadMeanRaw: AnglePoint = -moonHeadMeanReversed
    val moonHeadMean: AnglePoint = rounders.moonHeadMean(moonHeadMeanRaw)
    val moonTailMeanRaw: AnglePoint = moonHeadMeanRaw + Angle(180)
    val moonTailMean: AnglePoint = moonHeadMean + Angle(180)

    // KH 16:10
    val moonLatitudeCourseRaw: Angle = (moonLongitudeTrue - moonHeadMean).canonical
    val moonLatitudeCourse: Angle = rounders.moonLatitudeCourse(moonLatitudeCourseRaw)
    val isMoonLatitudeNortherly: Boolean = moonLatitudeCourse < Angle(180)
    val moonLatitude: Angle = calculators.moonLatitude(moonLatitudeCourse) // TODO AnglePoint

    // KH 17:1
    val longitude1: Angle = rounders.longitude1(moonLongitudeTrue - sunLongitudeTrue) // TODO AnglePoint
    // KH 17:2
    val latitude1: Angle = moonLatitude // TODO AnglePoint

    // KH 17:3-4
    val inNortherlyInclinedConstellations: Boolean = Zodiac.in(moonLongitudeTrue, Set(
      Zodiac.Capricorn, Zodiac.Aquarius, Zodiac.Pisces, Zodiac.Aries, Zodiac.Taurus, Zodiac.Gemini
    ))

    // KH 17:5-6
    val longitudeSightingAdjustment: Angle =
      calculators.moonLongitudeSightingAdjustment(moonLongitudeTrue)
    val longitude2: Angle = rounders.longitude2(longitude1 - longitudeSightingAdjustment) // TODO AnglePoint

    // KH 17:7-9
    val latitudeSightingAdjustment: Angle =
      calculators.moonLatitudeSightingAdjustment(moonLongitudeTrue)
    val latitude2: Angle =  // TODO AnglePoint
      if (isMoonLatitudeNortherly) latitude1 - latitudeSightingAdjustment
      else latitude1 + latitudeSightingAdjustment

    // KH 17:10
    val moonCircuitPortion: BigRational = calculators.moonCircuitPortion(moonLongitudeTrue)
    val moonCircuit: Angle = rounders.moonCircuit(latitude2*(moonCircuitPortion, defaultLength))

    // KH 17:11
    val longitude3: Angle = // TODO AnglePoint
      rounders.longitude3(if (
        (isMoonLatitudeNortherly && inNortherlyInclinedConstellations) ||
        (!isMoonLatitudeNortherly && !inNortherlyInclinedConstellations)
      ) longitude2 - moonCircuit else longitude2 + moonCircuit)

    // KH 17:12
    val moonLongitude3Portion: BigRational =
      calculators.moonLongitude3Portion(moonLongitudeTrue) /* TODO longitude3.toPoint?*/
    val moonLongitude3Correction: Angle =
      rounders.moonLongitude3Correction(longitude3*(moonLongitude3Portion, defaultLength))
    val longitude4: Angle = longitude3 + moonLongitude3Correction // TODO AnglePoint

    // KH 17:12
    val geographicCorrection: Angle =
      rounders.geographicCorrection(latitude1*(BigRational(2, 3), defaultLength))
    val arcOfSighting: Angle = rounders.arcOfSighting(
      if (isMoonLatitudeNortherly) longitude4 + geographicCorrection
      else longitude4 - geographicCorrection)

    // KH 17:3-4,15-21
    val isMoonSightable: Boolean =
      MoonSightable.forLongitude1(longitude1, inNortherlyInclinedConstellations).orElse(
      MoonSightable.forArcOfSighting(arcOfSighting)).getOrElse(
      MoonSightable.forSightingLimits(arcOfSighting, longitude1)
    )

    // TODO crescent calculations: KH 18-19!

    Calculator.Result(
      day,
      daysAfterEpoch,
      sunLongitudeMean,
      sunApogee,
      sunCourseRaw,
      sunCourse,
      sunLongitudeCorrection,
      sunLongitudeTrueRaw,
      sunLongitudeTrue,
      moonLongitudeMean,
      moonLongitudeAdjustmentForTimeOfSighting,
      moonLongitudeMeanAtTimeOfSighting,
      moonAnomalyMean,
      elongation,
      doubleElongation,
      moonLongitudeDoubleElongationCorrection,
      moonAnomalyTrueRaw,
      moonAnomalyTrue,
      moonAnomalyVisible,
      moonLongitudeTrueRaw,
      moonLongitudeTrue,
      moonHeadMeanReversed,
      moonHeadMeanRaw,
      moonHeadMean,
      moonTailMeanRaw,
      moonTailMean,
      moonLatitudeCourseRaw,
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
    sunCourseRaw: Angle,
    sunCourse: Angle,
    sunLongitudeCorrection: Angle,
    sunLongitudeTrueRaw: AnglePoint,
    sunLongitudeTrue: AnglePoint,
    moonLongitudeMean: AnglePoint,
    moonLongitudeAdjustmentForTimeOfSighting: Angle,
    moonLongitudeMeanAtTimeOfSighting: AnglePoint,
    moonAnomalyMean: AnglePoint,
    elongation: Angle,
    doubleElongation: Angle,
    moonLongitudeDoubleElongationCorrection: Angle,
    moonAnomalyTrueRaw: AnglePoint,
    moonAnomalyTrue: AnglePoint,
    moonAnomalyVisible: Angle,
    moonLongitudeTrueRaw: AnglePoint,
    moonLongitudeTrue: AnglePoint,
    moonHeadMeanReversed: AnglePoint,
    moonHeadMeanRaw: AnglePoint,
    moonHeadMean: AnglePoint,
    moonTailMeanRaw: AnglePoint,
    moonTailMean: AnglePoint,
    moonLatitudeCourseRaw: Angle,
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

  object Text extends Calculator(Epoch.Text, Calculators.Text, Rounders.Text)
}
