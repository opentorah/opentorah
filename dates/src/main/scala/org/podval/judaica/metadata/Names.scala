package org.podval.judaica.metadata

import scala.xml.Elem

final class Names(val names: Seq[Name]) extends Named.HasName {
  // No duplicates
  require(names.size == names.map(_.name).toSet.size, s"Different sizes: $names and ${names.map(_.name).toSet}")

  // TODO check that there are no duplicate combinations of parameters OTHER than name!

  def isEmpty: Boolean = names.isEmpty

  def find(name: String): Option[Name] = names.find(_.name == name)

  def hasName(name: String): Boolean = find(name).isDefined

  def find(spec: LanguageSpec): Option[Name] = names.find(_.satisfies(spec))

  def doFind(spec: LanguageSpec): Name =
    find(spec)
      .orElse(find(spec.dropFlavour))
      .orElse(find(spec.dropFlavour.dropIsTransliterated))
      .orElse(find(spec.dropFlavour.dropIsTransliterated.dropLanguage))
      .get

  override def toString: String = names.mkString("Names(", ", ", ")")

  def isDisjoint(other: Names): Boolean = names.forall(name => !other.hasName(name.name))
}

object Names {
  def checkDisjoint(nameses: Seq[Names]): Unit = {
    for {
      one <- nameses
      other <- nameses if !other.eq(one)
    } yield {
      require(one.isDisjoint(other), s"Names overlap: $one and $other")
    }
  }

  def merge(one: Names, other: Names): Names =
    if (other.isEmpty) one else throw new IllegalArgumentException(s"Merging Names not implemented: $one with $other")

  def parse(element: Elem, elementName: String): (Attributes, Option[Names], Seq[Elem]) = {
    val (attributes, elements) = XML.open(element, elementName)

    val defaultName: Option[Name] = attributes.get("n").map { defaultName => Name(defaultName, LanguageSpec.empty) }
    val (namesElements, tail) = XML.take(elements, "names")
    // TODO make a convenience method?
    require(namesElements.size <= 1, "Multiple 'names' elements.")
    val namesElement: Option[Elem] = namesElements.headOption
    val names: Option[Names] =
      if (namesElement.isEmpty) if (defaultName.isEmpty) None else Some(new Names(Seq(defaultName.get)))
      else Some(parse(namesElement.get, defaultName))

    (attributes, names, tail)
  }

  def parse(element: Elem, defaultName: Option[Name]): Names = {
    val (attributes, nameElements) = XML.open(element, "names")
    attributes.close()

    val nonDefaultNames: Seq[Name] = nameElements.map(parseName)
    val names = defaultName.fold(nonDefaultNames)(_ +: nonDefaultNames)
    new Names(names)
  }

  private def parseName(element: Elem): Name = {
    val (attributes, text: Option[String]) = XML.openText(element, "name")

    val n: Option[String] = attributes.get("n")
    require(n.isEmpty || text.isEmpty, "Both 'n' attribute and text are present.")
    val name: Option[String] = n.orElse(text)
    require(name.nonEmpty, "Both 'n' attribute and text are absent.")

    val result = Name(name.get, LanguageSpec(
      language = attributes.get("lang").map(Language.getForDefaultName),
      isTransliterated = attributes.getBoolean("transliterated"),
      flavour = attributes.get("flavour")
    ))

    attributes.close()

    result
  }
}
