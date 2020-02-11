package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.{Elem, Node}

private[xml] final class Element(
  url: Option[String], // TODO turn into URL and handle includes in Load/Parse
  name: String,
  attributes: Map[String, String],
  elements: Seq[Elem],
  nextElementNumber: Int,
  characters: Option[String]
) {
  override def toString: String =
    s"$name, before #$nextElementNumber ($getNextNestedElementName)" + url.fold("")(url => s"  from [$url]")

  def getName: String = name

  def takeAttribute(name: String): (Element, Option[String]) =
    (new Element(url, name, attributes - name, elements, nextElementNumber, characters), attributes.get(name))

  def takeCharacters: (Element, Option[String]) =
    (new Element(url, name, attributes, elements, nextElementNumber, characters = None), characters)

  def getNextNestedElementName: Option[String] =
    elements.headOption.map(_.label)

  def takeNextNestedElement: (Element, Elem) =
    (new Element(url, name, attributes, elements.tail, nextElementNumber + 1, characters), elements.head)

  def checkNoMixedContent: Parser[Unit] =
    Parse.check(elements.isEmpty || characters.isEmpty, s"Mixed content: [${characters.get}] $elements")

  def checkNoLeftovers: Parser[Unit] = for {
    _ <- Parse.check(elements.isEmpty, s"Unparsed elements: $elements")
    _ <- Parse.check(characters.isEmpty, s"Unparsed characters: ${characters.get}")
  } yield ()
}

object Element {

  def apply(url: Option[String], element: Elem): Element = {
    val (elements: Seq[Node], nonElements: Seq[Node]) = element.child.partition(_.isInstanceOf[Elem])
    new Element(
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
