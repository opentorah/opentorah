package org.podval.calendar.metadata

trait WithMetadata[M <: HasNames] extends Named {
  def metadata: M

  final override def names: Names = metadata.names
}
