package org.opentorah.util

import scala.ref.WeakReference

abstract class OneValueCache[T <: AnyRef] {
  private var value: Option[WeakReference[T]] = None

  final def set(result: T): Unit =
    value = Some(new WeakReference[T](result))

  final def get: T = value.flatMap(_.get).getOrElse {
    val result: T = calculate
    set(result)
    result
  }

  protected def calculate: T
}
