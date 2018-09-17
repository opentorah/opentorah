package org.podval.calendar.metadata

trait WithMetadata extends Named {

  type BindableMetadata <: Named.HasName

  type Metadata

  protected def toMetadata: Key => Metadata

  def bind(
    keys: Seq[Key],
    metadatas: Seq[BindableMetadata],
    parse: (Key, BindableMetadata) => Metadata
  ): Map[Key, Metadata] = {
    require(keys.length == metadatas.length)

    // TODO disjoint

    // TODO relax the "same order" requirement.
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.hasName(key.name))
      key -> parse(key, metadata)
    }.toMap
  }
}
