package org.podval.calendar.metadata

trait WithNames extends Named with WithResource {

  trait KeyBase extends Names.NamedBase { this: Key =>
    final override def names: Names = toNames(this)
  }

  override type Key <: KeyBase

  // This is lazy to allow Language to initialize correctly: its metadata file references Language instances by name :)
  final lazy val toNames: Map[Key, Names] = {
    val url = MetadataParser.getUrl(this, resourceName)
    val elements = MetadataParser.loadMetadataElements(url, "names", resourceName)
    val metadatas: Seq[Names] = elements.map(element => NamesParser.parseNamesElement(element, None))
    Names.checkDisjoint(metadatas)

    // TODO relax the "same order" requirement.
    // TODO merge into bind()?
    require(values.length == metadatas.length, s"Different lengths: $values and $metadatas")
    val result = values.zip(metadatas).map { case (key, metadata) =>
      require(metadata.has(key.name))
      key -> metadata
    }

    result.toMap
  }
}
