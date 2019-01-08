package org.podval.calendar

/** Number System.
  *
  * Each number system `S` (derived from [[org.podval.calendar.numbers.Number]])
  * supports two flavors of [[org.podval.calendar.numbers.Number]]:
  * `S#Point` (derived from [[org.podval.calendar.numbers.PointBase]] and
  * `S#Vector` (derived from [[org.podval.calendar.numbers.VectorBase]].
  *
  * This distinction may seem to be artificial:
  * after all, assuming operations `-: Point x Point -> Vector` with right unit `z` and
  * `+: Point x Vector -> Point`, we have an isomorphism between
  * `Points` and `Vectors`: `ptov(p) = p - z`, `vtop(v) = z + v`.
  *
  * Indeed, there is not much difference between the two when we are talking about angles.
  * However, for dates (`Point`s) it makes sense to ask what year/month the date is in -
  * but not for time intervals (`Vector`s)!
  *
  * This distinction allows definitions of operations to be constrained to the types where
  * a give operation makes sense.
  * To avoid using the subversive isomorphism described above in the code using the `Numbers`,
  * certain operations have to be defined as fundamental, e.g.,
  * [[org.podval.calendar.numbers.PeriodicNumber.reflect]].
  *
  */
package object numbers {
}
