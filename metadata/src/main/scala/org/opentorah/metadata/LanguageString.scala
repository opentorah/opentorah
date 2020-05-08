package org.opentorah.metadata

trait LanguageString {
  final override def toString: String = toLanguageString(LanguageSpec.empty)

  def toLanguageString(implicit spec: LanguageSpec): String
}
