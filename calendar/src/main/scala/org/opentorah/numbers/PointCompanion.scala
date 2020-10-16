package org.opentorah.numbers

abstract class PointCompanion[S <: Numbers[S]](numbers: S) extends NumberCompanion[S, S#Point](numbers) {
  protected final override def isCanonical: Boolean = true
}
