package org.opentorah.metadata

trait WithNames {
  def names: Names
  
  def merge(that: WithNames): WithNames = {
    require(this == that)
    this
  }

  final def toLanguageString(implicit spec: LanguageSpec): String = names.toLanguageString
}
