package org.opentorah.astronomy

import org.opentorah.angles.Angles.{Position, Rotation}

trait Rounders:
  def sunCourse: Rotation => Rotation

  def sunLongitudeTrue: Position => Position

  def moonAnomalyTrue: Position => Position

  def moonAnomalyVisible: Rotation => Rotation

  def moonLongitudeTrue: Position => Position

  def moonHeadMean: Position => Position

  def moonLatitudeCourse: Rotation => Rotation

  def longitude1: Rotation => Rotation

  def longitude2: Rotation => Rotation

  def moonCircuit: Rotation => Rotation

  def longitude3: Rotation => Rotation

  def moonLongitude3Correction: Rotation => Rotation

  def geographicCorrection: Rotation => Rotation

  def arcOfSighting: Rotation => Rotation


object Rounders:
  object Text extends Rounders:
    final override def sunCourse: Rotation => Rotation = _.roundToDegrees // KH 13:9
    final override def sunLongitudeTrue: Position => Position = _.roundToMinutes // KH 13:10
    final override def moonAnomalyTrue: Position => Position = _.roundToDegrees // KH 15:8
    final override def moonLongitudeTrue: Position => Position = _.roundToMinutes // KH 13:10
    final override def moonAnomalyVisible: Rotation => Rotation = _.roundToMinutes // KH 13:10
    final override def moonHeadMean: Position => Position = _.roundToMinutes // KH 13:10
    final override def moonLatitudeCourse: Rotation => Rotation = _.roundToDegrees // KH 16:19
    final override def longitude1: Rotation => Rotation = _.roundToMinutes // KH 13:10
    final override def longitude2: Rotation => Rotation =  _.roundToMinutes // KH 13:10
    final override def moonCircuit: Rotation => Rotation = _.roundToMinutes // KH 17:14
    final override def longitude3: Rotation => Rotation = _.roundToMinutes // KH 13:10
    final override def moonLongitude3Correction: Rotation => Rotation = _.roundToMinutes // KH 13:10
    final override def geographicCorrection: Rotation => Rotation = _.roundToMinutes // KH 13:10
    final override def arcOfSighting: Rotation => Rotation = _.roundToMinutes // KH 13:10

  object Identity extends Rounders:
    final override def sunCourse: Rotation => Rotation = identity
    final override def sunLongitudeTrue: Position => Position = identity
    final override def moonAnomalyTrue: Position => Position = identity
    final override def moonLongitudeTrue: Position => Position = identity
    final override def moonAnomalyVisible: Rotation => Rotation = identity
    final override def moonHeadMean: Position => Position = identity
    final override def moonLatitudeCourse: Rotation => Rotation = identity
    final override def longitude1: Rotation => Rotation = identity
    final override def longitude2: Rotation => Rotation =  identity
    final override def moonCircuit: Rotation => Rotation = identity
    final override def longitude3: Rotation => Rotation = identity
    final override def moonLongitude3Correction: Rotation => Rotation = identity
    final override def geographicCorrection: Rotation => Rotation = identity
    final override def arcOfSighting: Rotation => Rotation = identity
