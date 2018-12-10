package org.podval.judaica.metadata

trait Named extends WithName with WithNames {
  override def name: String = Util.className(this)

  override def toString: String = name

  final def toLanguageString(implicit spec: LanguageSpec): String = names.doFind(spec).name
}
