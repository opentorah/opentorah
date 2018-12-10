package org.podval.judaica.metadata

import scala.xml.Elem

final class Names(val names: Seq[Name]) extends HasName {
  Util.checkNoDuplicates(names.map(_.name), "names")

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

  def transform(transformer: Name => Name): Names = new Names(names.map(transformer))
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

  // TODO inline
  def merge(one: Names, other: Names): Names =
    if (other.isEmpty) one else throw new IllegalArgumentException(s"Merging Names not implemented: $one with $other")


  // TODO there is a lot of similarities with Custom...
  def combine(one: Names, other: Names, combiner: (LanguageSpec, String, String) => String): Names = {
    val specs: Set[LanguageSpec] = one.names.map(_.languageSpec).toSet ++ other.names.map(_.languageSpec)
    val result: Set[Name] = specs.map { spec => Name(combiner(spec, one.doFind(spec).name, other.doFind(spec).name), spec) }
    new Names(result.toSeq)
  }

  def parse(attributes: Attributes, elements: Seq[Elem]): (Names, Seq[Elem]) = {
    val defaultName: Option[Name] = parseDefaultName(attributes)
    val (nameElements, tail) = XML.take(elements, "name")
    val names: Names =
      if (nameElements.isEmpty) new Names(Seq(defaultName.get))
      else parse(nameElements, defaultName)

    (names, tail)
  }

  private def parseDefaultName(attributes: Attributes): Option[Name] =
    attributes.get("n").map { defaultName => Name(defaultName, LanguageSpec.empty) }

  def parse(element: Elem, defaultName: Option[Name]): Names = {
    val (attributes, nameElements) = XML.open(element, "names")
    attributes.close()
    parse(nameElements, defaultName)
  }

  private def parse(nameElements: Seq[Elem], defaultName: Option[Name]): Names = {
    val nonDefaultNames: Seq[Name] = nameElements.map(parseName)
    val names = defaultName.fold(nonDefaultNames){ defaultName =>
      // TODO drop default name if it is contained in the non-default ones
      defaultName +: nonDefaultNames
    }
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
