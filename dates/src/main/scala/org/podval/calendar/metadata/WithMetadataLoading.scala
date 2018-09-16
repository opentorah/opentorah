package org.podval.calendar.metadata

import java.net.URL

import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed

trait WithMetadataLoading extends WithMetadata with WithResource {
  // This is lazy to allow correct initialization: the code uses values()...
  final override protected lazy val toMetadata: Map[Key, Metadata] = {
    val url = MetadataParser.getUrl(this, resourceName)
    val metadatas: Seq[MetadataPreparsed] = MetadataParser.loadMetadata(url, resourceName, elementName)

    bind(values, metadatas).map { case (key, metadata) =>
      key -> parseMetadata(url, key, metadata)
    }.toMap
  }

  protected def elementName: String

  protected def parseMetadata(
    url: URL,
    key: Key,
    metadata: MetadataPreparsed
  ): Metadata
}
