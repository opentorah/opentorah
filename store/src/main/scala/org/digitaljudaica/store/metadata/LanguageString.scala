package org.digitaljudaica.store.metadata

trait LanguageString {
  final override def toString: String = toLanguageString(LanguageSpec.empty)

  def toLanguageString(implicit spec: LanguageSpec): String
}
