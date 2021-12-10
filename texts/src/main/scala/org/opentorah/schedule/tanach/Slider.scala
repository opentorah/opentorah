package org.opentorah.schedule.tanach

class Slider[Q, S, T](seq: Seq[Q], isCurrent: (Q, S) => Boolean, value: Q => T):
  private var current: Seq[Q] = seq

  final def reset(): Unit = current = seq

  final def get(s: S): T =
    current = current.dropWhile(q => !isCurrent(q, s))
    value(current.head)

final class PairSlider[S, T](seq: Seq[(S, T)], isCurrent: (S, S) => Boolean) extends Slider[(S, T), S, T](seq,
  isCurrent = { case ((qs, _), s) => isCurrent(qs, s) },
  value = { case (_, qt) => qt }
)
