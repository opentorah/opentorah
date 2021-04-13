package org.opentorah.docbook.section

import java.io.File
import org.opentorah.mathjax.MathJaxConfiguration

final class NonOverridableParameters(
  val isInfoEnabled: Boolean,
  val embeddedFonts: String,
  val cssFile: String,
  val imagesDirectoryName: String,
  val useMathJax3: Boolean, // TODO carry MathJax here
  val mathJaxConfiguration: Option[MathJaxConfiguration],
  val documentName: String,
  val saxonOutputDirectory: File
)
