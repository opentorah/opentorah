package org.podval.judaica.metadata

trait LanguageString {
  final override def toString: String = toLanguageString(LanguageSpec.empty)

  def toLanguageString(implicit spec: LanguageSpec): String
}
