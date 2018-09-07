package org.podval.calendar.metadata

final class Names(val names: Seq[Name]) {
  // No duplicates
  require(names.size == names.map(_.name).toSet.size)
  // TODO check that there are no duplicate combinations of parameters OTHER than name!

  def isEmpty: Boolean = names.isEmpty

  def find(name: String): Option[Name] = names.find(_.name == name)

  def has(name: String): Boolean = find(name).isDefined

  def find(
    lang: Option[String],
    isTransliterated: Option[Boolean],
    flavour: Option[String]
  ): Option[Name] = names.find { name =>
    (lang.isEmpty || (name.lang == lang)) &&
    (isTransliterated.isEmpty || (name.isTransliterated == isTransliterated)) &&
    (flavour.isEmpty || (name.flavour == flavour))
  }

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
