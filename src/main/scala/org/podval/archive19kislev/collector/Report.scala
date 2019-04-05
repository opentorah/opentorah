package org.podval.archive19kislev.collector

final class Report(
  val collections: Seq[CollectionReport],
  val names: NamesReport
 ) {
  def failed: Boolean = names.failed || collections.exists(_.failed)

  def toStrings: Seq[String] = Seq(
    "---",
    "title: Status report",
    "layout: page",
    "---"
  ) ++
    names.toStrings ++
    (for (collection <- collections) yield collection.toStrings).flatten
}

object Report {
  val NONE: Seq[String] = Seq("None")

  def filterUndefined(references: Set[Name]): Set[String] =
    references.filter(_.ref.isEmpty).map(_.name)

  def filterUnresolved(references: Set[Name], names: Names): Set[String] =
    references.filter(_.ref.isDefined).map(_.ref.get).filter(names.find(_).isEmpty)

  def forUndefined(references: Set[String]): Seq[String] =
    for (reference <- references.toSeq) yield s"- 'Name>$reference</'"

  def forUnresolved(references: Set[String]): Seq[String] =
    Seq("### Unresolved references ###") ++
      (if (references.isEmpty) NONE else {
        for (reference <- references.toSeq) yield s"- $reference"
      })

  def apply(collections: Seq[Collection], names: Names): Report = new Report(
    collections = collections.map(CollectionReport(_, names)),
    names = NamesReport(names)
  )
}
