package org.digitaljudaica.xml

import scala.xml.{Elem, Node}

final case class Element(
  url: Option[String],
  name: String,
  attributes: Map[String, String],
  elements: Seq[Elem],
  nextElementNumber: Int,
  characters: Option[String]
) {
  def getAttribute(name: String): Option[String] = attributes.get(name)

  def forgetAttribute(name: String): Element = copy(attributes = attributes - name)

  def forgetCharacters: Element = copy(characters = None)

  def getNextNestedElementName: Option[String] = elements.headOption.map(_.label)

  def getNumberOfNestedElementsWithName(name: String): Int = elements.takeWhile(_.label == name).size

  def getNextNestedElement: Elem = elements.head

  def moveToNextNestedElement: Element = copy(elements = elements.tail, nextElementNumber = nextElementNumber + 1)
}

object Element {

  def apply(url: Option[String], element: Elem): Element = {
    val (elements: Seq[Node], nonElements: Seq[Node]) = element.child.partition(_.isInstanceOf[Elem])
    Element(
      url = url,
      name = element.label,
      attributes = element.attributes.map(metadata => metadata.key -> metadata.value.toString).toMap,
      elements = elements.map(_.asInstanceOf[Elem]),
      nextElementNumber = 0,
      characters = if (nonElements.isEmpty) None else {
        val result: String = nonElements.map(_.text).mkString.trim
        if (result.isEmpty) None else Some(result)
      }
    )
  }
}
