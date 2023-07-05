package org.opentorah.astronomy

import org.opentorah.util.Collections
import Angles.{Position, Rotation}

abstract class MapTable[K, V](values: (K, String)*)(string2v: String => V):
  private val map: Map[K, V] = Collections.mapValues(values.toMap)(string2v)

  final def value(key: K): V = map(key)

object MapTable:
  abstract class RotationMapTable[K](values: (K, String)*) extends MapTable[K, Rotation](values*)(Rotation(_))

  abstract class ZodiacRotationMapTable(values: (Zodiac, String)*) extends RotationMapTable[Zodiac](values*):
    final def calculate(position: Position): Rotation = value(Zodiac.forPosition(position))
