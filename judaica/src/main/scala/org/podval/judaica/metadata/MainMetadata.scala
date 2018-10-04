package org.podval.judaica.metadata

trait MainMetadata extends HasMetadata {
  trait KeyBase extends Named.NamedBase { this: Key =>
    final def metadata: Metadata = toMetadata(this)

    final override def names: Names = metadata.names
  }

  override type Key <: KeyBase

  override type Metadata <: Named.NamedBase
}
