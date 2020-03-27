package org.opentorah.store

final case class WithPath[T](
  path: Path,
  value: T
)
