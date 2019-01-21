package org.podval.calendar.numbers

trait PointCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Point] {
  protected final override def isCanonical: Boolean = true
}
