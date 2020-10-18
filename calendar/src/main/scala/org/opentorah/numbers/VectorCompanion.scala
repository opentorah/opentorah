package org.opentorah.numbers

trait VectorCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Vector] {
  protected final override def isCanonical: Boolean = false

  private[numbers] final def canonical(digits: Digits): S#Vector =
    numbers.Vector.newNumber(numbers.normalize(digits, isCanonical = true))
}
