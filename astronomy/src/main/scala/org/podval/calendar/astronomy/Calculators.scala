package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.numbers.BigRational

trait Calculators {
  def sunLongitudeMean: Int => Angle

  def sunApogee: Int => Angle

  def sunLongitudeCorrection: Angle => Angle

  def moonLongitudeMean: Int => Angle

  def moonLongitudeAdjustmentForTimeOfSighting: AnglePoint => Angle

  def moonAnomalyMean: Int => Angle

  def moonLongitudeDoubleElongationCorrection: Angle => Angle

  def moonAnomalyVisible: Angle => Angle

  def moonHeadMean: Int => Angle

  def moonLatitude: Angle => Angle

  def moonLongitudeSightingAdjustment: AnglePoint => Angle

  def moonLatitudeSightingAdjustment: AnglePoint => Angle

  def moonCircuitPortion: AnglePoint => BigRational

  def moonLongitude3Portion: AnglePoint => BigRational
}


object Calculators {
  object Text extends Calculators {
    final override def sunLongitudeMean: Int => Angle = SunLongitudeMean.table.calculate
    final override def sunApogee: Int => Angle = SunApogee.table.calculate
    final override def sunLongitudeCorrection: Angle => Angle = SunLongitudeCorrection.table.calculate
    final override def moonLongitudeMean: Int => Angle = MoonLongitudeMean.table.calculate
    final override def moonLongitudeAdjustmentForTimeOfSighting: AnglePoint => Angle =
      MoonLongitudeAdjustmentForTimeOfSighting.calculate
    final override def moonAnomalyMean: Int => Angle = MoonAnomalyMean.table.calculate
    final override def moonLongitudeDoubleElongationCorrection: Angle => Angle =
      MoonLongitudeDoubleElongationCorrection.calculate
    final override def moonAnomalyVisible: Angle => Angle = MoonAnomalyVisible.table.calculate
    final override def moonHeadMean: Int => Angle = MoonHeadMean.table.calculate
    final override def moonLatitude: Angle => Angle = MoonLatitude.table.calculate
    final override def moonLongitudeSightingAdjustment: AnglePoint => Angle =
      MoonLongitudeSightingAdjustment.calculate
    final override def moonLatitudeSightingAdjustment: AnglePoint => Angle =
      MoonLatitudeSightingAdjustment.calculate
    final override def moonCircuitPortion: AnglePoint => BigRational = MoonCircuitPortion.calculate
    final override def moonLongitude3Portion: AnglePoint => BigRational =
      MoonLongitude3Portion.calculate
  }

  // TODO hather all misprints into object Misprints extends Calculators
}
