package org.podval.judaica.util

abstract class Cache[S, T] {
  private val values = new collection.mutable.WeakHashMap[S, T]

  final def get(s: S): T = values.getOrElseUpdate(s, calculate(s))

  protected def calculate(s: S): T
}
