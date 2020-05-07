package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles.{Position, Rotation}
import org.opentorah.numbers.BigRational

trait Calculators {
  def sunLongitudeMean: Int => Rotation

  def sunApogee: Int => Rotation

  def sunLongitudeCorrection: Rotation => Rotation

  def moonLongitudeMean: Int => Rotation

  def moonLongitudeAdjustmentForTimeOfSighting: Position => Rotation

  def moonAnomalyMean: Int => Rotation

  def moonLongitudeDoubleElongationCorrection: Rotation => Rotation

  def moonAnomalyVisible: Position => Rotation

  def moonHeadMean: Int => Rotation

  def moonLatitude: Rotation => Rotation

  def moonLongitudeSightingAdjustment: Position => Rotation

  def moonLatitudeSightingAdjustment: Position => Rotation

  def moonCircuitPortion: Position => BigRational

  def moonLongitude3Portion: Position => BigRational
}


object Calculators {
  class TextLike extends Calculators {
    override def sunLongitudeMean: Int => Rotation = SunLongitudeMean.calculate
    override def sunApogee: Int => Rotation = SunApogee.calculate
    override def sunLongitudeCorrection: Rotation => Rotation = SunLongitudeCorrection.table.calculate
    override def moonLongitudeMean: Int => Rotation = MoonLongitudeMean.calculate
    override def moonLongitudeAdjustmentForTimeOfSighting: Position => Rotation = MoonLongitudeAdjustmentForTimeOfSighting.calculate
    override def moonAnomalyMean: Int => Rotation = MoonAnomalyMean.calculate
    override def moonLongitudeDoubleElongationCorrection: Rotation => Rotation = MoonLongitudeDoubleElongationCorrection.calculate
    override def moonAnomalyVisible: Position => Rotation = MoonAnomalyVisible.table.calculate
    override def moonHeadMean: Int => Rotation = MoonHeadMean.calculate
    override def moonLatitude: Rotation => Rotation = MoonLatitude.table.calculate
    override def moonLongitudeSightingAdjustment: Position => Rotation = MoonLongitudeSightingAdjustment.calculate
    override def moonLatitudeSightingAdjustment: Position => Rotation = MoonLatitudeSightingAdjustment.calculate
    override def moonCircuitPortion: Position => BigRational = MoonCircuitPortion.calculate
    override def moonLongitude3Portion: Position => BigRational = MoonLongitude3Portion.calculate
  }

  object Text extends TextLike

  object Misprints extends TextLike {
    override def moonAnomalyVisible: Position => Rotation = MoonAnomalyVisible.misprinted.calculate
  }
}
