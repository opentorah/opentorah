package org.digitaljudaica.xml

import cats.data.State
import cats.implicits._
import scala.xml.{Elem, Node}

private[xml] sealed trait Content

private[xml] object Content {

  // TODO monadize :) (State[Content, A])
  type Modifier[A] = Content => ErrorOr[(Content, A)]

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

  val takeCharacters: Modifier[Option[String]] = {
    case Characters(characters) =>
      Right((Characters(None), characters))

    case Mixed(nextElementNumber, nodes) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      if (elements.nonEmpty) Left(s"Elements in $this")
      else Right((Mixed(nextElementNumber, Seq.empty), toCharacters(nonElements)))

    case content => Left(s"No characters in $content")
  }

  val getNextElementName: Content => Option[String] = {
    case Elements(_, elements) =>
      elements.headOption.map(_.label)

    case Mixed(_, nodes) =>
      dropWhitespace(nodes).headOption.flatMap {
        case result: Elem => Some(result.label)
        case _ => None
      }

    case _ => None
  }

  val takeNextElement: Modifier[Option[Elem]] = {
    case content@Elements(nextElementNumber, elements) => Right {
      elements.headOption.fold[(Content, Option[Elem])]((content, None)) { result =>
        (Elements(nextElementNumber + 1, elements.tail), Some(result))
      }
    }

    case content@Mixed(nextElementNumber, nodes) => Right {
      val noLeadingWhitespace = dropWhitespace(nodes)
      noLeadingWhitespace.headOption.fold[(Content, Option[Elem])]((content, None)) {
        case result: Elem => (Mixed(nextElementNumber + 1, noLeadingWhitespace.tail), Some(result))
        case _ => (content, None)
      }
    }

    case content => Left(s"No element in $content")
  }

  val takeAllNodes: Modifier[Seq[Node]] = {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      Right(Elements(nextElementNumber, Seq.empty), elements)

    case Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      Right(Mixed(nextElementNumber, Seq.empty), nodes)

    case content => Left(s"No nodes in $content")
  }

  val takeAllElements: Modifier[Seq[Elem]] = {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      Right(Elements(nextElementNumber, Seq.empty), elements)

    case content@Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      val hasNonWhitespace: Boolean = nonElements.exists(node => !isWhitespace(node))
      if (hasNonWhitespace) Left(s"Non white-space nodes in $content")
      else Right((Mixed(nextElementNumber, Seq.empty), elements))

    case content => Left(s"No elements in $content")
  }

  private val ok: ErrorOr[Unit] = Right(())

  val checkNoLeftovers: Content => ErrorOr[Unit] = {
    case Empty => ok

    case Characters(characters) =>
      characters.fold[ErrorOr[Unit]](ok)(characters => Left(s"Unparsed characters: $characters"))

    case Elements(_, elements) =>
      if (elements.isEmpty) ok else Left(s"Unparsed elements: $elements")

    case Mixed(_, nodes) =>
      if (nodes.isEmpty) ok else Left(s"Unparsed nodes: $nodes")
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
