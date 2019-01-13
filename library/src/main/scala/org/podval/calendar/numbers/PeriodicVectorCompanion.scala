package org.podval.calendar.numbers

trait PeriodicVectorCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Vector] {
  protected final override def autoCanonicalize: Boolean = false
}
