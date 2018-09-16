package org.podval.calendar.metadata

trait WithKeyedMetadata extends WithMetadata {
  trait MetadataBase extends Names.NamedBase { this: Key =>
    final def metadata: Metadata = toMetadata(this)

    final override def names: Names = metadata.names
  }

  override type Key <: MetadataBase

  override type Metadata <: Names.NamedBase
}
