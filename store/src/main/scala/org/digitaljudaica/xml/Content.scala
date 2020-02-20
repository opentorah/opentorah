package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.{Elem, Node}

private[xml] sealed trait Content

private[xml] object Content {

  private final case object Empty extends Content

  private final case class Characters(characters: Option[String]) extends Content

  private final case class Elements(nextElementNumber: Int, elements: Seq[Elem]) extends Content

  private final case class Mixed(nextElementNumber: Int, nodes: Seq[Node]) extends Content

  def open(nodes: Seq[Node], contentType: ContentType): ErrorOr[Content] = {
    val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
    val characters: Option[String] = toCharacters(nonElements)

    contentType match {
      case ContentType.Empty =>
        if (elements.nonEmpty) Left(s"Spurious elements: $elements")
        else if (characters.nonEmpty) Left(s"Spurious characters: '${characters.get}'")
        else Right(Empty)

      case ContentType.Text =>
        if (elements.nonEmpty) Left(s"Spurious elements: $elements")
        else Right(Characters(characters))

      case ContentType.Elements =>
        if (characters.nonEmpty) Left(s"Spurious characters: '${characters.get}'")
        else Right(Elements(0, elements))

      case ContentType.Mixed =>
        Right(Mixed(0, nodes))
    }
  }

  def takeCharacters(content: Content): ErrorOr[(Content, Option[String])] = content match {
    case Characters(characters) =>
      Right((Characters(None), characters))

    case Mixed(nextElementNumber, nodes) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      if (elements.nonEmpty) Left(s"Elements in $this")
      else Right((Mixed(nextElementNumber, Seq.empty), toCharacters(nonElements)))

    case _ => Left(s"No characters in $content")
  }

  def takeNextElement(content: Content): ErrorOr[(Content, Elem)] = content match {
    case Elements(nextElementNumber, elements) =>
      elements.headOption.fold[ErrorOr[(Content, Elem)]](Left(s"No element in $content")) { result =>
        Right((Elements(nextElementNumber+1, elements.tail), result))
      }

    case Mixed(nextElementNumber, nodes) =>
      val noLeadingWhitespace = dropWhitespace(nodes)
      noLeadingWhitespace.headOption.fold[ErrorOr[(Content, Elem)]](Left(s"No element in $content")) {
        case result: Elem => Right(Mixed(nextElementNumber + 1, noLeadingWhitespace.tail), result)
        case _ => Left(s"No element in $content")
      }

    case _ => Left(s"No element in $content")
  }

  def getNextElementName(content: Content): Option[String] = content match {
    case Elements(_, elements) =>
      elements.headOption.fold[Option[String]](None) { result => Some(result.label) }

    case Mixed(_, nodes) =>
      dropWhitespace(nodes).headOption.fold[Option[String]](None) {
        case result: Elem => Some(result.label)
        case _ => None
      }

    case _ => None
  }

  def takeAllNodes(content: Content): ErrorOr[(Content, Seq[Node])] = content match {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      Right(Elements(nextElementNumber, Seq.empty), elements)

    case Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      Right(Mixed(nextElementNumber, Seq.empty), nodes)

    case _ => Left(s"No nodes in $content")
  }

  def takeAllElements(content: Content): ErrorOr[(Content, Seq[Elem])] = content match {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      Right(Elements(nextElementNumber, Seq.empty), elements)

    case Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      val hasNonWhitespace: Boolean = nonElements.exists(node => !isWhitespace(node))
      if (hasNonWhitespace) Left(s"Non white-space nodes in $content")
      else Right((Mixed(nextElementNumber, Seq.empty), elements))

    case _ => Left(s"No elements in $content")
  }

  def checkNoLeftovers(content: Content): ErrorOr[Unit] = content match {
    case Empty => Right(())

    case Characters(characters) =>
      characters.fold[ErrorOr[Unit]](Right(()))(characters => Left(s"Unparsed characters: $characters"))

    case Elements(_, elements) =>
      if (elements.isEmpty) Right(()) else Left(s"Unparsed elements: $elements")

    case Mixed(_, nodes) =>
      if (nodes.isEmpty) Right(()) else Left(s"Unparsed nodes: $nodes")
  }

  private def partition(nodes: Seq[Node]): (Seq[Elem], Seq[Node]) =
    nodes.partition(_.isInstanceOf[Elem]).leftMap(_.map(_.asInstanceOf[Elem]))

  private def toCharacters(nodes: Seq[Node]): Option[String] = {
    val result = nodes.map(_.text).mkString.trim
    if (result.isEmpty) None else Some(result)
  }

  private def dropWhitespace(nodes: Seq[Node]): Seq[Node] =
    nodes.dropWhile(isWhitespace)

  private def isWhitespace(node: Node): Boolean =
    node.text.trim.isEmpty
}
