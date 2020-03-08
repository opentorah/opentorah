package org.opentorah.calendar.numbers

trait VectorCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Vector] {
  protected final override def isCanonical: Boolean = false

  private[numbers] final def canonical(digits: Seq[Int]): S#Vector =
    newNumber(numbers.normalize(digits, isCanonical = true))
}
