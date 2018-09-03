package org.podval.calendar.generate.tanach

import scala.xml.Elem

object NamesParser {
  def doParse(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Names, Seq[Elem]) = {
    val (attributes, names, elements) = parse(element, name, allowedAttributes)
    (attributes, names.get, elements)
  }

  def parse(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Option[Names], Seq[Elem]) = {
    val (attributes, elements) = XML.open(element, name, allowedAttributes)
    val defaultName: Option[Name] = attributes.get("n").map {
      defaultName => Name(defaultName, None, None, None)
    }
    val (namesElements, tail) = XML.span(elements, "names")
    // TODO make a convenience method?
    if (namesElements.size > 1) throw new IllegalArgumentException("Multiple 'names' elements.")
    val namesElement: Option[Elem] = namesElements.headOption
    val names: Seq[Name] = if (namesElement.isEmpty)
      if (defaultName.isEmpty) Seq.empty else Seq(defaultName.get)
    else {
      val (_, nameElements) = XML.open(namesElement.get, "names", Set.empty)
      val nonDefaultNames: Seq[Name] = nameElements.map(parseName)
      defaultName.fold(nonDefaultNames)(_ +: nonDefaultNames)
    }

    (attributes, if (names.isEmpty) None else Some(new Names(names)) , tail)
  }

  private def parseName(element: Elem): Name = {
    val attributes = XML.openEmpty(element, "name", Set("n", "lang", "transliterated", "flavour"))
    val n: Option[String] = attributes.get("n")
    val text: Option[String] = XML.text(element)
    if (n.nonEmpty && text.nonEmpty) throw new IllegalArgumentException("Both 'n' attribute and text are present.")
    val name: Option[String] = n.orElse(text)
    if (name.isEmpty) throw new IllegalArgumentException("Both 'n' attribute and text are absent.")
    Name(
      name.get,
      lang = attributes.get("lang"),
      isTransliterated = XML.getBooleanAttribute(attributes, "transliterated"),
      flavour = attributes.get("flavour")
    )
  }
}
