package org.podval.calendar.generate.tanach

import java.io.{FileNotFoundException, Writer, File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter}
import java.net.URL

import scala.xml.{Elem, Node, Utility, PrettyPrinter}

object XML {
  val baseUrl: URL = getURL("Tanach")

  def getURL(name: String): URL = getClass.getClassLoader.getResource(name)

  private def childURL(parent: URL, name: String): URL = new URL(
    parent.getProtocol, parent.getHost, parent.getPort, parent.getFile + "/" + name, null)

  def childFileURL(parent: URL, name: String): URL = childURL(parent, name + ".xml")

  def loadResource(url: URL): Option[Elem] = {
    try {
      val result = xml.XML.load(url.openStream())
      Some(Utility.trimProper(result).asInstanceOf[Elem])
    } catch {
      case _: FileNotFoundException => None
    }
  }

  def loadSubresource(element: Elem, elementName: String, baseUrl: URL): (Names, Seq[Elem]) = {
    val (attributes: Map[String, String], names: Names, elements: Seq[Elem]) = doParseNames(element, elementName, Set("n"))

    if (elements.nonEmpty) (names, elements) else {
      val subresources: Seq[Elem] = names.names.flatMap(name => loadResource(childFileURL(baseUrl, name.name)))
      require(subresources.size <= 1, "More than one subresource.")
      if (subresources.isEmpty) (names, elements) else {
        val (_, newNames, newElements) = parseNames(subresources.head, elementName, Set("n"))
        (
          newNames.fold(names)(Names.merge(names, _)),
          newElements
        )
      }
    }
  }

  def doParseNames(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Names, Seq[Elem]) = {
    val (attributes, names, elements) = parseNames(element, name, allowedAttributes)
    (attributes, names.get, elements)
  }

  private def parseNames(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Option[Names], Seq[Elem]) = {
    val (attributes, elements) = open(element, name, allowedAttributes)
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
      val (_, nameElements) = open(namesElement.get, "names", Set.empty)
      val nonDefaultNames: Seq[Name] = nameElements.map(parseName)
      defaultName.fold(nonDefaultNames)(_ +: nonDefaultNames)
    }

    (attributes, if (names.isEmpty) None else Some(new Names(names)) , tail)
  }

  private def parseName(element: Elem): Name = {
    val attributes = openEmpty(element, "name", Set("n", "lang", "transliterated", "flavour"))
    val n: Option[String] = attributes.get("n")
    val text: Option[String] = getText(element)
    require(n.isEmpty || text.isEmpty, "Both 'n' attribute and text are present.")
    val name: Option[String] = n.orElse(text)
    require(name.nonEmpty, "Both 'n' attribute and text are absent.")
    Name(
      name.get,
      lang = attributes.get("lang"),
      isTransliterated = getBooleanAttribute(attributes, "transliterated"),
      flavour = attributes.get("flavour")
    )
  }

  private def open(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Seq[Elem]) = {
    val attributes = check(element, name, allowedAttributes)

    val (elements, nonElements) = element.child.partition(_.isInstanceOf[Elem])
    require(nonElements.isEmpty, "Non-element children present.")

    (attributes, elements.map(_.asInstanceOf[Elem]))
  }

  // TODO make vararg for the allowedAttributes?
  def openEmpty(element: Elem, name: String, allowedAttributes: Set[String]): Map[String, String] = {
    val attributes = check(element, name, allowedAttributes)

    val (elements, _) = element.child.partition(_.isInstanceOf[Elem])
    require(elements.isEmpty, "Nested elements present.")

    attributes
  }

  private def check(element: Elem, name: String, allowedAttributes: Set[String]): Map[String, String] = {
    require(element.label == name, s"Wrong element: ${element.label} instead of $name")
    val attributes = getAttributes(element)
    attributes.foreach { case (key, _) => require(allowedAttributes.contains(key), s"Attribute $key not allowed.") }
    attributes
  }

  private def getAttributes(element: Elem): Map[String, String] = element.attributes.map { metadata =>
    val key = metadata.key
    val value = metadata.value.toString
    key -> value
  }.toMap

  def getIntAttribute(attributes: Map[String, String], name: String): Option[Int] = {
    attributes.get(name).map { attribute =>
      val result = attribute.toInt
      require(result > 0, s"Non-positive integer: $result")
      result
    }
  }

  def doGetIntAttribute(attributes: Map[String, String], name: String): Int =
    getIntAttribute(attributes, name).get

  def getBooleanAttribute(attributes: Map[String, String], name: String): Option[Boolean] =
    attributes.get(name).map { value =>
      if ((value == "true") || (value == "yes")) true
      else if ((value == "false") || (value == "no")) false
      else throw new IllegalArgumentException("Bad boolean attribute value")
    }

  def doGetBooleanAttribute(attributes: Map[String, String], name: String): Boolean =
    getBooleanAttribute(attributes, name).getOrElse(false)

  def checkMeta(element: Elem, what: String): Seq[Elem] = {
    val (attributes, elements) = open(element, "meta", Set("type"))
    val typeOption = attributes.get("type")
    require(typeOption.nonEmpty, "Attribute 'type' missing.")
    require(typeOption.get == what, "Wrong type.")
    elements
  }

  private def getText(element: Elem): Option[String] = {
    val result = element.text
    if (result.isEmpty) None else Some(result)
  }

  def span(elements: Seq[Elem], name1: String): (Seq[Elem], Seq[Elem]) = {
    elements.span(_.label == name1)
  }

  def span(elements: Seq[Elem], name1: String, name2: String): (Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    checkNoMoreElements(tail2)
    (elements1, elements2)
  }

  def span(elements: Seq[Elem], name1: String, name2: String, name3: String): (Seq[Elem], Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    val (elements3, tail3) = tail2.span(_.label == name3)
    checkNoMoreElements(tail3)
    (elements1, elements2, elements3)
  }

  def checkNoMoreElements(elements: Seq[Elem]): Unit =
    require(elements.isEmpty, s"Spurious elements: ${elements.head.label}")

  def print(xml: Node, outStream: OutputStream): Unit = print(xml, new OutputStreamWriter(outStream))
  def print(xml: Node, outFile: File): Unit = print(xml, new FileWriter(outFile))

  def print(xml: Node, writer: Writer) {
    val out = new PrintWriter(writer)
    val pretty = prettyPrinter.format(xml)
    // TODO when outputting XML, include <xml> header?
    out.println(pretty)
    out.close()
  }

  private val prettyPrinter = new PrettyPrinter(120, 4)
}
