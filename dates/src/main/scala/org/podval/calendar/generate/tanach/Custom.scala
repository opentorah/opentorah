package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.{Metadata, MetadataParser, WithMetadata}

sealed trait Custom extends WithMetadata[Metadata] {
  final override def metadata: Metadata = Custom.name2value(this)
}

object Custom {

  case object Chabad extends Custom
  case object Ashkenaz extends Custom
  case object Sefard extends Custom


  val all: Seq[Custom] = Seq(Chabad, Ashkenaz, Sefard)

  private val name2value = MetadataParser.loadNames(this, all, "custom")
}
