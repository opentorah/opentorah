package org.podval.judaica.metadata

trait Named extends HasNames {
  def name: String = Util.className(this)

  override def toString: String = name

  final def toString(spec: LanguageSpec): String = names.doFind(spec).name
}
