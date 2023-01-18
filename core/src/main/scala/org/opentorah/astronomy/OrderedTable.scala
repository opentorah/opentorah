package org.opentorah.astronomy

import org.opentorah.util.Collections

abstract class OrderedTable[S, N <: Angles.Angle[N], V](values: (S, String)*)(s2n: S => N, string2v: String => V):

  private val valuesSorted: Seq[(N, V)] = values
    .map((s: S, r: String) => (s2n(s), string2v(r)))
    .sortBy((n: N, _: V) => n)

  final def find(argument: N): V = valuesSorted
    .takeWhile(_._1 <= argument) // TODO use dropWhile>/headOption?
    .lastOption
    .map(_._2)
    .getOrElse(throw IllegalArgumentException(s"Not found for $argument"))

  final protected def bracket(argument: N): ((N, V), Option[(N, V)]) =
    // TODO redo with takeWhile()
    val (allBefore: Seq[(N, V)], allAfter: Seq[(N, V)]) = valuesSorted.span(_._1 <= argument)
    (allBefore.last, allAfter.headOption)
