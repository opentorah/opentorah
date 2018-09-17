package org.podval.calendar.metadata

trait WithKeyedMetadata extends WithMetadata {
  trait KeyBase extends Named.NamedBase { this: Key =>
    final def metadata: Metadata = toMetadata(this)

    final override def names: Names = metadata.names
  }

  override type Key <: KeyBase

  override type Metadata <: Named.NamedBase
}
