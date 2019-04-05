package org.podval.archive19kislev.collector

final class CollectionReport(
  val collection: Collection,
  val undefinedReferences: Map[Document, Set[String]],
  val malformedReferences: Map[Document, Set[String]]
) {
  def failed: Boolean = undefinedReferences.nonEmpty || malformedReferences.nonEmpty

  def toStrings: Seq[String] =
    Seq(s"## Collection '$collection' ##") ++
    Report.UNDEFINED ++
    (if (undefinedReferences.isEmpty) Report.NONE else {
      (for ((document, references) <- undefinedReferences) yield
        Seq(s"#### Document $document ####") ++
          Report.forUndefined(references)).flatten
    }) ++
    Report.MALFORMED ++
    (if (malformedReferences.isEmpty) Report.NONE else {
      (for ((document, references) <- malformedReferences) yield
        Seq(s"#### Document $document ####") ++
          Report.forMalformed(references)).flatten
    })
}

object CollectionReport {
  def apply(collection: Collection, names: Names): CollectionReport = new CollectionReport(
    collection = collection,
    undefinedReferences = (for {
      document <- collection.documents
      result = Report.filterUndefined(document.references.toSet)
      if result.nonEmpty
    } yield (document, result)).toMap,
    malformedReferences = (for {
      document <- collection.documents
      result = Report.filterMalformed(document.references.toSet)
      if result.nonEmpty
    } yield (document, result)).toMap
  )
}
