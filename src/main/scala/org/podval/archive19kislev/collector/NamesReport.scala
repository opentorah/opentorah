package org.podval.archive19kislev.collector

final class NamesReport(
  val undefinedReferences: Set[String],
  val unresolvedReferences: Set[String]
) {
  def failed: Boolean = undefinedReferences.nonEmpty || unresolvedReferences.nonEmpty

  def toStrings: Seq[String] = Seq(
    "## Names List ##",
    "### Undefined references ###"
  ) ++
    (if (undefinedReferences.isEmpty) Report.NONE else Report.forUndefined(undefinedReferences)) ++
    Report.forUnresolved(unresolvedReferences)
}

object NamesReport {
  def apply(names: Names): NamesReport = new NamesReport(
    undefinedReferences = Report.filterUndefined(names.references),
    unresolvedReferences = Report.filterUnresolved(names.references, names)
  )
}
