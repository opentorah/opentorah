package org.podval.calendar.metadata

final class Names(val names: Seq[Name]) {
  // No duplicates
  require(names.size == names.map(_.name).toSet.size, s"Different sizes: $names and ${names.map(_.name).toSet}")
  // TODO check that there are no duplicate combinations of parameters OTHER than name!

  def isEmpty: Boolean = names.isEmpty

  def find(name: String): Option[Name] = names.find(_.name == name)

  def has(name: String): Boolean = find(name).isDefined

  def find(spec: LanguageSpec): Option[Name] = names.find(_.satisfies(spec))

  def doFind(spec: LanguageSpec): Name =
    find(spec)
      .orElse(find(spec.dropFlavour))
      .orElse(find(spec.dropFlavour.dropIsTransliterated))
      .orElse(find(spec.dropFlavour.dropIsTransliterated.dropLanguage))
      .get

  override def toString: String = names.mkString("Names(", ", ", ")")

  def isDisjoint(other: Names): Boolean = names.forall(name => !other.has(name.name))
}

object Names {
  def checkDisjoint(nameses: Seq[Names]): Unit = {
    for {
      one <- nameses
      other <- nameses if !other.eq(one)
    } yield {
      if (!one.isDisjoint(other)) throw new IllegalArgumentException("Names overlap.")
    }
  }

  def merge(one: Names, other: Names): Names =
    if (other.isEmpty) one else throw new IllegalArgumentException("Merging Names not implemented.")
}
