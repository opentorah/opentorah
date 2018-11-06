package org.podval.judaica.metadata

import java.io._

import scala.xml.{Elem, Node, PrettyPrinter, Text}

object XML {
  def open(element: Elem, name: String): (Attributes, Seq[Elem]) = {
    checkName(element, name)
    checkNoNonElements(element)
    (Attributes(element), getElements(element))
  }

  // TODO make private and use only via parseEmpty
  def openEmpty(element: Elem, name: String): Attributes = {
    checkName(element, name)
    checkNoElements(element)
    checkNoNonElements(element)
    Attributes(element)
  }

  def openText(element: Elem, name: String): (Attributes, Option[String]) = {
    checkName(element, name)
    checkNoElements(element)
    val text = element.text
    (Attributes(element), if (text.isEmpty) None else Some(text))
  }

  private def checkName(element: Elem, name: String): Unit =
    require(element.label == name, s"Wrong element: ${element.label} instead of $name")

  private def checkNoElements(element: Elem): Unit =
    require(getElements(element).isEmpty, "Nested elements present.")

  private def checkNoNonElements(element: Elem): Unit = {
    val nonElements = getNonElements(element)
    require(nonElements.isEmpty, s"Non-element children present on element $element: $nonElements")
  }

  private def getElements(element: Elem): Seq[Elem] =
    element.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  private def getNonElements(element: Elem): Seq[Node] = {
    element.child.filterNot(_.isInstanceOf[Elem]).filter { node =>
      !node.isInstanceOf[Text] ||
      node.asInstanceOf[Text].text.replace('\n', ' ').trim.nonEmpty
    }
  }

  def take(elements: Seq[Elem], name1: String): (Seq[Elem], Seq[Elem]) = {
    elements.span(_.label == name1)
  }

  def parseEmpty[T](element: Elem, name: String, parser: Attributes => T): T = {
    val attributes = XML.openEmpty(element, name)
    val result = parser(attributes)
    attributes.close()
    result
  }

  def noMoreThanOne(elements: Seq[Elem]): Option[Elem] = {
    require(elements.length <= 1)
    elements.headOption
  }

  def span(elements: Seq[Elem], name1: String): Seq[Elem] = {
    val (result, tail) = take(elements, name1)
    checkNoMoreElements(tail)
    result
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
