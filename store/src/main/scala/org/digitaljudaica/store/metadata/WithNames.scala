package org.digitaljudaica.store.metadata

trait WithNames extends HasName {
  def names: Names

  final override def hasName(name: String): Boolean = names.hasName(name)

  def merge(that: WithNames): WithNames = {
    require(this == that)
    this
  }

  final def toLanguageString(implicit spec: LanguageSpec): String = names.toLanguageString
}
