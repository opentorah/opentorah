package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

trait Rounders {
  def sunCourse: Angle => Angle

  def sunLongitudeTrue: AnglePoint => AnglePoint

  def moonAnomalyTrue: AnglePoint => AnglePoint

  def moonAnomalyVisible: Angle => Angle

  def moonLongitudeTrue: AnglePoint => AnglePoint

  def moonHeadMean: AnglePoint => AnglePoint

  def moonLatitudeCourse: Angle => Angle

  def longitude1: Angle => Angle

  def longitude2: Angle => Angle

  def moonCircuit: Angle => Angle

  def longitude3: Angle => Angle

  def moonLongitude3Correction: Angle => Angle

  def geographicCorrection: Angle => Angle

  def arcOfSighting: Angle => Angle
}


object Rounders {
  object Text extends Rounders {
    final override def sunCourse: Angle => Angle = _.roundToDegrees // KH 13:9
    final override def sunLongitudeTrue: AnglePoint => AnglePoint = _.roundToMinutes // KH 13:10
    final override def moonAnomalyTrue: AnglePoint => AnglePoint = _.roundToDegrees // KH 15:8
    final override def moonLongitudeTrue: AnglePoint => AnglePoint = _.roundToMinutes // KH 13:10
    final override def moonAnomalyVisible: Angle => Angle = _.roundToMinutes // KH 13:10
    final override def moonHeadMean: AnglePoint => AnglePoint = _.roundToMinutes // KH 13:10
    final override def moonLatitudeCourse: Angle => Angle = _.roundToDegrees // KH 16:19
    final override def longitude1: Angle => Angle = _.roundToMinutes // KH 13:10
    final override def longitude2: Angle => Angle =  _.roundToMinutes // KH 13:10
    final override def moonCircuit: Angle => Angle = _.roundToMinutes // KH 17:14
    final override def longitude3: Angle => Angle = _.roundToMinutes // KH 13:10
    final override def moonLongitude3Correction: Angle => Angle = _.roundToMinutes // KH 13:10
    final override def geographicCorrection: Angle => Angle = _.roundToMinutes // KH 13:10
    final override def arcOfSighting: Angle => Angle = _.roundToMinutes // KH 13:10
  }

  object Identity extends Rounders {
    final override def sunCourse: Angle => Angle = identity
    final override def sunLongitudeTrue: AnglePoint => AnglePoint = identity
    final override def moonAnomalyTrue: AnglePoint => AnglePoint = identity
    final override def moonLongitudeTrue: AnglePoint => AnglePoint = identity
    final override def moonAnomalyVisible: Angle => Angle = identity
    final override def moonHeadMean: AnglePoint => AnglePoint = identity
    final override def moonLatitudeCourse: Angle => Angle = identity
    final override def longitude1: Angle => Angle = identity
    final override def longitude2: Angle => Angle =  identity
    final override def moonCircuit: Angle => Angle = identity
    final override def longitude3: Angle => Angle = identity
    final override def moonLongitude3Correction: Angle => Angle = identity
    final override def geographicCorrection: Angle => Angle = identity
    final override def arcOfSighting: Angle => Angle = identity
  }
}
