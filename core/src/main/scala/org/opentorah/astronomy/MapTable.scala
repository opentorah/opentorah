package org.opentorah.astronomy

import org.opentorah.util.Collections

abstract class MapTable[K, V](values: (K, String)*)(string2v: String => V):
  private val map: Map[K, V] = Collections.mapValues(values.toMap)(string2v)

  final def value(key: K): V = map(key)
