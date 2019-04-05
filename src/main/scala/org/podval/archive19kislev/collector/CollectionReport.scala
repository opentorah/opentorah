package org.podval.archive19kislev.collector

final class CollectionReport(
  val collection: Collection,
  val undefinedReferences: Map[Document, Set[String]],
  val unresolvedReferences: Set[String]
) {
  def failed: Boolean = undefinedReferences.nonEmpty || unresolvedReferences.nonEmpty

  def toStrings: Seq[String] = Seq(
    s"## Collection '$collection' ##",
    "### Undefined references ###"
  ) ++
    (if (undefinedReferences.isEmpty) Report.NONE else {
      (for ((document, references) <- undefinedReferences) yield
        Seq(s"#### Document $document ####") ++
          Report.forUndefined(references)).flatten
    }) ++
    Report.forUnresolved(unresolvedReferences)
}

object CollectionReport {
  def apply(collection: Collection, names: Names): CollectionReport = new CollectionReport(
    collection = collection,
    undefinedReferences = (for {
      document <- collection.documents
      result = Report.filterUndefined(document.references.toSet)
      if result.nonEmpty
    } yield (document, result)).toMap,
    unresolvedReferences = Report.filterUnresolved(collection.documents.toSet
      .flatMap((document: Document) => document.references.toSet), names)
  )
}
