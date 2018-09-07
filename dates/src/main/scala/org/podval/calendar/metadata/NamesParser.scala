package org.podval.calendar.metadata

import org.podval.calendar.metadata.XML.{open, openEmpty, span}

import scala.xml.Elem

// TODO generalize and merge into MetadataParser
object NamesParser {
  def parseNames(element: Elem, name: String): (Attributes, Option[Names], Seq[Elem]) = {
    val (attributes, elements) = open(element, name)

    val defaultName: Option[Name] = attributes.get("n").map {
      defaultName => Name(defaultName, None, None, None)
    }
    val (namesElements, tail) = span(elements, "names")
    // TODO make a convenience method?
    require(namesElements.size <= 1, "Multiple 'names' elements.")
    val namesElement: Option[Elem] = namesElements.headOption
    val names: Seq[Name] = if (namesElement.isEmpty)
      if (defaultName.isEmpty) Seq.empty else Seq(defaultName.get)
    else {
      val (attributes, nameElements) = open(namesElement.get, "names")
      attributes.close()

      val nonDefaultNames: Seq[Name] = nameElements.map(parseName)
      defaultName.fold(nonDefaultNames)(_ +: nonDefaultNames)
    }

    (attributes, if (names.isEmpty) None else Some(new Names(names)) , tail)
  }

  private def parseName(element: Elem): Name = {
    val (attributes, text: Option[String]) = XML.openText(element, "name")
    val n: Option[String] = attributes.get("n")
    require(n.isEmpty || text.isEmpty, "Both 'n' attribute and text are present.")
    val name: Option[String] = n.orElse(text)
    require(name.nonEmpty, "Both 'n' attribute and text are absent.")
    val result = Name(
      name.get,
      lang = attributes.get("lang"),
      isTransliterated = attributes.getBoolean("transliterated"),
      flavour = attributes.get("flavour")
    )
    attributes.close()
    result
  }
}
