package org.opentorah.numbers

trait NonPeriodicNumbers extends Numbers {
  final override def headRangeOpt: Option[Int] = None
}
