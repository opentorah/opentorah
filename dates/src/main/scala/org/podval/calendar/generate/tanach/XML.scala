package org.podval.calendar.generate.tanach

import java.io.{FileNotFoundException, Writer, File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter}
import java.net.URL

import scala.xml.{Elem, Node, Utility, PrettyPrinter}

object XML {
  val baseUrl: URL = getURL("Tanach")

  def getURL(name: String): URL = getClass.getClassLoader.getResource(name)

  def childURL(parent: URL, name: String): URL = new URL(
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

  def open(element: Elem, name: String, allowedAttributes: Set[String]): (Map[String, String], Seq[Elem]) = {
    val attributes = check(element, name, allowedAttributes)

    val (elements, nonElements) = element.child.partition(_.isInstanceOf[Elem])
    if (nonElements.nonEmpty) throw new IllegalArgumentException("Non-element children present.")

    (attributes, elements.map(_.asInstanceOf[Elem]))
  }

  // TODO make vararg for the allowedAttributes?
  def openEmpty(element: Elem, name: String, allowedAttributes: Set[String]): Map[String, String] = {
    val attributes = check(element, name, allowedAttributes)

    val (elements, _) = element.child.partition(_.isInstanceOf[Elem])
    if (elements.nonEmpty) throw new IllegalArgumentException("Nested elements present.")

    attributes
  }

  private def check(element: Elem, name: String, allowedAttributes: Set[String]): Map[String, String] = {
    if (element.label != name) throw new IllegalArgumentException(s"Wrong element: ${element.label} instead of $name")
    val attributes = getAttributes(element)
    attributes.foreach { case (key, _) =>
      if (!allowedAttributes.contains(key))
        throw new IllegalArgumentException(s"Attribute $key not allowed.")
    }
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
      if (result <= 0) throw new IllegalArgumentException(s"Non-positive integer: $result")
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
    if (typeOption.isEmpty) throw new IllegalArgumentException("Attribute 'type' missing.")
    if (typeOption.get != what) throw new IllegalArgumentException("Wrong type.")
    elements
  }

  def text(element: Elem): Option[String] = {
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
    if (elements.nonEmpty) throw new IllegalArgumentException(s"Spurious elements: ${elements.head.label}")

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
