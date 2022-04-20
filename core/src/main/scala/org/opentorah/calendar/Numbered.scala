package org.opentorah.calendar

trait Numbered[T] extends org.opentorah.metadata.Numbered[T]:
  def companion: Numbered.Companion[T]

  final def +(change: Int): T = companion(number + change)

  final def -(change: Int): T = companion(number - change)

  final def next: T = this + 1

  final def prev: T = this - 1

  // TODO declare so that the one in DayBase is covered - or maybe move into metadata.Numbered?
  // final def -(that: Numbered[T]): Int = this.number - that.number

object Numbered:

  trait Companion[T]:
    def apply(number: Int): T
