package org.podval.calendar.metadata

trait WithMetadata[K <: WithMetadata[K, M], M <: HasNames] extends Named { this: K =>
  final def metadata: M = toMetadata(this)

  final override def names: Names = metadata.names

  def toMetadata: Map[K, M]
}
