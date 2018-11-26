package org.podval.judaica.metadata

// TODO dissolve; remove use of WithName from APIs; rename WithName to Named.
trait Named extends WithName with WithNames {
  override def name: String = Util.className(this)

  override def toString: String = name

  final def toLanguageString(implicit spec: LanguageSpec): String = names.doFind(spec).name
}
