package org.podval.archive19kislev.collector

final class Report(
  val collections: Seq[CollectionReport],
  val names: NamesReport,
  val unresolvedReferences: Set[String]
 ) {
  def failed: Boolean = names.failed || collections.exists(_.failed) || unresolvedReferences.nonEmpty

  def toStrings: Seq[String] =
    names.toStrings ++
    (for (collection <- collections) yield collection.toStrings).flatten ++
    Seq("## Unresolved references ##") ++
    (if (unresolvedReferences.isEmpty) Report.NONE
     else for (reference <- unresolvedReferences.toSeq) yield s"""- ref="#$reference" """)
}

object Report {
  val NONE: Seq[String] = Seq("None")
  val UNDEFINED: Seq[String] = Seq("### Undefined references ###")
  val MALFORMED: Seq[String] = Seq("### Malformed references ###")

  def filterUndefined(references: Set[Name]): Set[String] =
    references.filterNot(_.isMalformed).filter(_.ref.isEmpty).map(_.name)

  def filterMalformed(references: Set[Name]): Set[String] =
    references.filter(_.isMalformed).filter(_.ref.isDefined).map(_.ref.get)

  def forUndefined(references: Set[String]): Seq[String] =
    for (reference <- references.toSeq) yield s"- Name>$reference</"

  def forMalformed(references: Set[String]): Seq[String] =
    for (reference <- references.toSeq) yield s"""- ref="$reference" """

  def apply(collections: Seq[Collection], names: Names): Report = {
    val documentReferences: Set[Name] =
      (for {
        collection <- collections
        document <- collection.documents
      } yield document.references).flatten.toSet

    new Report(
      collections = collections.map(CollectionReport(_, names)),
      names = NamesReport(names),
      unresolvedReferences = (documentReferences ++ names.references)
        .filterNot(_.isMalformed)
        .filter(_.ref.isDefined).map(_.ref.get)
        .filter(names.find(_).isEmpty)
    )
  }
}
