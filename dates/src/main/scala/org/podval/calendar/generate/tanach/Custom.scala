package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{MetadataParser, Names, WithNames}

sealed trait Custom extends WithNames[Custom] {
  final override def toNames: Map[Custom, Names] = Custom.toNames
}

object Custom {
  case object Chabad extends Custom
  case object Ashkenaz extends Custom
  case object Sefard extends Custom

  val all: Seq[Custom] = Seq(Chabad, Ashkenaz, Sefard)

  private val toNames: Map[Custom, Names] = MetadataParser.loadNames(this, all)
}
