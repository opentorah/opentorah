package org.podval.calendar

/** Number System.
  *
  * Each number system `S` (derived from [[org.podval.calendar.numbers.Number]])
  * supports two flavors of [[org.podval.calendar.numbers.Number]]:
  * `S#Point` (derived from [[org.podval.calendar.numbers.PointNumber]] and
  * `S#Vector` (derived from [[org.podval.calendar.numbers.VectorNumber]].
  *
  * This distinction allows definitions of operations to be constrained to the types where
  * a give operation makes sense:
  * `-: (Vector, Vector) => Vector` with right unit `Vector.zero`;
  * `+: (Vector, Vector) => Vector` with unit `Vector.zero`;
  * `-: (Point, Point) => Vector`;
  * `+: (Point, Vector) => Point` with left unit `Point.zero` and its "commutation"
  * `+: (Vector, Point) => Point` with right unit `Point.zero`.
  *
  * This distinction may seem to be artificial:
  * after all, given operations `-: (Point, Point) => Vector` and `+: (Point, Vector) => Point`,
  * we have an isomorphism between `Points` and `Vectors`: `ptov(p) = p - Point.zero`, `vtop(v) = Point.zero + v`.
  *
  * Indeed, there is not much difference between the two when we are talking about angles.
  * However, for dates (`Point`s) it makes sense to ask what year/month the date is in -
  * but not for time intervals (`Vector`s)!
  */
package object numbers {
}
