package org.podval.calendar.generate.tanach

import scala.xml.Elem

object NamesParser {
  // TODO return attributes also!
  def doParse(elem: Elem, name: String, allowedAttributes: Set[String]): (Names, Seq[Elem]) = {
    val (names, elements) = parse(elem, name, allowedAttributes)
    (names.get, elements)
  }

  def parse(elem: Elem, name: String, allowedAttributes: Set[String]): (Option[Names], Seq[Elem]) = {
    val attributes = XML.checkElement(elem, name, allowedAttributes)
    val defaultName: Option[Name] = attributes.get("n").map {
      defaultName => Name(defaultName, None, None, None)
    }
    val elements: Seq[Elem] = XML.children(elem)
    if (!elements.headOption.exists(_.label == "names")) {
      if (defaultName.isEmpty) (None, elements) else {
        val names = Seq(defaultName.get)
        (Some(new Names(names)), elements)
      }
    } else {
      val namesElement: Elem = elements.head
      XML.checkElement(namesElement, "names", Set.empty)
      val nonDefaultNames: Seq[Name] = XML.children(namesElement).map(parseName)
      val names = defaultName.fold(nonDefaultNames)(_ +: nonDefaultNames)
      (Some(new Names(names)), elements.tail)
    }
  }

  private def parseName(elem: Elem): Name = {
    val attributes = XML.checkElement(elem, "name", Set("n", "lang", "transliterated", "flavour"))
    val n: Option[String] = attributes.get("n")
    val lang: Option[String] = attributes.get("lang")
    val isTransliterated: Option[Boolean] = XML.getBooleanAttribute(attributes, "transliterated")
    val flavour: Option[String] = attributes.get("flavour")
    val text: Option[String] = XML.text(elem)
    if (n.nonEmpty && text.nonEmpty) throw new IllegalArgumentException("Both 'n' attribute and text are present.")
    val name: Option[String] = n.orElse(text)
    if (name.isEmpty) throw new IllegalArgumentException("Both 'n' attribute and text are absent.")
    Name(name.get, lang, isTransliterated, flavour)
  }
}
