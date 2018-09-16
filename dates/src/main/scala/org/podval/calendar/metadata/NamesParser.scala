package org.podval.calendar.metadata

import org.podval.calendar.metadata.XML.{open, take}

import scala.xml.Elem

// TODO generalize and merge into MetadataParser
object NamesParser {
  def parseNames(element: Elem, name: String): (Attributes, Option[Names], Seq[Elem]) = {
    val (attributes, elements) = open(element, name)

    val defaultName: Option[Name] = attributes.get("n").map { defaultName => Name(defaultName, LanguageSpec.empty) }
    val (namesElements, tail) = take(elements, "names")
    // TODO make a convenience method?
    require(namesElements.size <= 1, "Multiple 'names' elements.")
    val namesElement: Option[Elem] = namesElements.headOption
    val names: Option[Names] =
      if (namesElement.isEmpty) if (defaultName.isEmpty) None else Some(new Names(Seq(defaultName.get)))
      else Some(parseNamesElement(namesElement.get, defaultName))

    (attributes, names, tail)
  }

  def parseNamesElement(element: Elem, defaultName: Option[Name]): Names = {
    val (attributes, nameElements) = open(element, "names")
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
