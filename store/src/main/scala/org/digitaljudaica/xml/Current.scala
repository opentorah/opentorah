package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.{Elem, Node}

private[xml] final class Current(
  url: Option[From], // TODO handle includes in Load/Parse
  name: String,
  attributes: Map[String, String],
  elements: Seq[Elem],
  nextElementNumber: Int,
  characters: Option[String]
) {
  override def toString: String =
    s"$name, before #$nextElementNumber ($getNextNestedElementName)" + url.fold("")(url => s"  from [$url]")

  def getName: String = name

  def takeAttribute(name: String): (Current, Option[String]) =
    (new Current(url, name, attributes - name, elements, nextElementNumber, characters), attributes.get(name))

  def takeCharacters: (Current, Option[String]) =
    (new Current(url, name, attributes, elements, nextElementNumber, characters = None), characters)

  def getNextNestedElementName: Option[String] =
    elements.headOption.map(_.label)

  def takeNextNestedElement: (Current, Elem) =
    (new Current(url, name, attributes, elements.tail, nextElementNumber + 1, characters), elements.head)

  def checkNoMixedContent: Parser[Unit] =
    Check(elements.isEmpty || characters.isEmpty, s"Mixed content: [${characters.get}] $elements")

  def checkNoLeftovers: Parser[Unit] = for {
    // TODO check attributes too!
    _ <- Check(characters.isEmpty, s"Unparsed characters: ${characters.get}")
    _ <- Check(elements.isEmpty, s"Unparsed elements: $elements")
  } yield ()
}

private[xml] object Current {

  def apply(url: Option[From], element: Elem): Current = {
    val (elements: Seq[Node], nonElements: Seq[Node]) = element.child.partition(_.isInstanceOf[Elem])
    new Current(
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
