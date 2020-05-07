package org.opentorah.numbers

trait NonPeriodicNumbers[S <: NonPeriodicNumbers[S]] extends Numbers[S] { this: S =>
  final override def headRangeOpt: Option[Int] = None
}
