package org.opentorah.calendar

// For stand-alone testing of Times.
object SimpleTimes extends Times {
  override type Point = TimePointBase

  override type PointCompanionType = PointCompanion

  override protected def createPointCompanion: PointCompanionType = new PointCompanion

  override protected def newPoint(digits: Seq[Int]): Point = new TimePointBase(digits)
}
