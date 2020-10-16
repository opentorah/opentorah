package org.opentorah.numbers

abstract class VectorCompanion[S <: Numbers[S]](numbers: S) extends NumberCompanion[S, S#Vector](numbers) {
  protected final override def isCanonical: Boolean = false

  private[numbers] final def canonical(digits: Digits): S#Vector =
    newNumber(numbers.normalize(digits, isCanonical = true))
}
