package org.podval.archive19kislev.collector

final class NamesReport(
  val undefinedReferences: Set[String],
  val malformedReferences: Set[String]
) {
  def failed: Boolean = undefinedReferences.nonEmpty || malformedReferences.nonEmpty

  def toStrings: Seq[String] =
    Seq("## Names List ##") ++
    Report.UNDEFINED ++
    (if (undefinedReferences.isEmpty) Report.NONE else Report.forUndefined(undefinedReferences)) ++
    Report.MALFORMED ++
    (if (malformedReferences.isEmpty) Report.NONE else Report.forMalformed(malformedReferences))
}

object NamesReport {
  def apply(names: Names): NamesReport = new NamesReport(
    undefinedReferences = Report.filterUndefined(names.references),
    malformedReferences = Report.filterMalformed(names.references)
  )
}
