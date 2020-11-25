package org.opentorah.collector

import org.opentorah.tei.EntityReference
import org.opentorah.util.Files

sealed class ReferenceWithSource(val path: Seq[String], val reference: EntityReference)

object ReferenceWithSource {

  final case class FromEntity(
    override val path: Seq[String],
    override val reference: EntityReference,
    entityId: String,
    entityName: String
  ) extends ReferenceWithSource(path, reference)

  final case class FromDocument(
    override val path: Seq[String],
    override val reference: EntityReference,
    collectionFileName: String,
    collectionName: String,
    documentName: String
  ) extends ReferenceWithSource(path, reference) {
    def shortUrl: String = Files.mkUrl(shortPath)
    def shortPath: Seq[String] = Files.addExtension(
      Seq(CollectionObject.directoryName, collectionFileName, CollectionObject.documentsDirectoryName, documentName),
      "html"
    )
  }

  final case class FromElement(
    override val path: Seq[String],
    override val reference: EntityReference,
  ) extends ReferenceWithSource(path, reference)
}
