package org.podval.judaica.viewer

trait Named {
  def names: Names

  final def defaultName: String = names.default.name
}


final class Name(val name: String, val lang: String, val isTransliterated: Boolean) {
  override def toString: String =
    "Name: " + name + " (" + lang + (if (!isTransliterated) "" else ", " + isTransliterated) +  ")"
}


final class Names(val names: Seq[Name]) {
  def find(name: String): Option[Name] = find(names, name)

  private[this] def find(names: Seq[Name], name: String): Option[Name] = names.find(_.name == name)

  def has(name: String): Boolean = find(name).isDefined

  def byLang(lang: Language): Option[Name] = names.find(_.lang == lang.name)

  def default: Name = names.head

  def isEmpty: Boolean = names.isEmpty

  override def toString: String = "Names: " + names
}


object Names {
  def find[T <: Named](nameds: Iterable[T], name: String): Option[T] = nameds.find(_.names.has(name))

  def doFind[T <: Named](nameds: Iterable[T], name: String, what: String): T = Exists(find(nameds, name), name, what)
}
