package org.podval.judaica.metadata

trait WithNames extends HasName {
  def names: Names

  final override def hasName(name: String): Boolean = names.hasName(name)
}
