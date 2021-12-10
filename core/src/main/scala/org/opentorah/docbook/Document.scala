package org.opentorah.docbook

import org.opentorah.math.MathConfiguration

final class Document(
  val name: String,
  val output: Set[Variant],
  val parameters: Map[String, String],
  val math: Option[MathConfiguration],
  val substitutions: Map[String, String],
  val xslt1version: Option[String],
  val xslt2version: Option[String],
  val epubEmbeddedFonts: List[String],
  val dataGeneratorClass: Option[String],
  val imagesDirectory: Option[String]
) extends HasName
