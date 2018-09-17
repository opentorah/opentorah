package org.podval.calendar.metadata

trait WithMetadata extends Named {

  type Metadata

  protected def toMetadata: Key => Metadata

  def bind[M <: Named.HasNames](keys: Seq[Key], metadatas: Seq[M]): Seq[(Key, M)] = {
    require(keys.length == metadatas.length)
    // TODO relax the "same order" requirement.
    // TODO disjoint
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.hasName(key.name))
      key -> metadata
    }
  }
}
