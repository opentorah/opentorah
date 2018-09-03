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

  def checkElement(elem: Elem, name: String, allowedAttributes: Set[String]): Map[String, String] = {
    if (elem.label != name) throw new IllegalArgumentException(s"Wrong element: ${elem.label} instead of $name")
    val result = getAttributes(elem)
    result.foreach { case (key, _) =>
      if (!allowedAttributes.contains(key))
        throw new IllegalArgumentException(s"Attribute $key not allowed.")
    }
    result
  }

  def getAttributes(elem: Elem): Map[String, String] = elem.attributes.map { metadata =>
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

  def getBooleanAttribute(attributes: Map[String, String], name: String): Option[Boolean] =
    attributes.get(name).map { value =>
      if ((value == "true") || (value == "yes")) true
      else if ((value == "false") || (value == "no")) false
      else throw new IllegalArgumentException("Bad boolean attribute value")
    }

  def checkMeta(elem: Elem, what: String): Elem = {
    checkElement(elem, "meta", Set("type"))
    val typeOption = getAttributes(elem).get("type")
    if (typeOption.isEmpty) throw new IllegalArgumentException("Attribute 'type' missing.")
    if (typeOption.get != what) throw new IllegalArgumentException("Wrong type.")
    elem
  }

  def children(elem: Elem): Seq[Elem] = {
    val (result, nonElements) = elem.child.partition(_.isInstanceOf[Elem])
    if (nonElements.nonEmpty) throw new IllegalArgumentException("Non-element children present.")
    result.map(_.asInstanceOf[Elem])
  }

  def text(elem: Elem): Option[String] = {
    val result = elem.text
    if (result.isEmpty) None else Some(result)
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
