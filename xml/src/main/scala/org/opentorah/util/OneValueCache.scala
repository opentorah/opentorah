package org.opentorah.util

import scala.ref.WeakReference

// TODO move to what uses it
abstract class OneValueCache[T <: AnyRef] {
  private var value: Option[WeakReference[T]] = None

  final def get: T = value.flatMap(_.get).getOrElse {
    val result: T = calculate
    value = Some(new WeakReference[T](result))
    result
  }

  protected def calculate: T
}
