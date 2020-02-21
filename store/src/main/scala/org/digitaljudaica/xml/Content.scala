package org.digitaljudaica.xml

import scala.xml.{Elem, Node}
import zio.IO

private[xml] sealed trait Content

private[xml] object Content {

  type Modifier[A] = Content => IO[Error, (Content, A)]

  private final case object Empty extends Content

  private final case class Characters(characters: Option[String]) extends Content

  private final case class Elements(nextElementNumber: Int, elements: Seq[Elem]) extends Content

  private final case class Mixed(nextElementNumber: Int, nodes: Seq[Node]) extends Content

  def open(nodes: Seq[Node], contentType: ContentType): IO[Error, Content] = {
    val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
    val characters: Option[String] = toCharacters(nonElements)

    contentType match {
      case ContentType.Empty =>
        if (elements.nonEmpty) IO.fail(s"Spurious elements: $elements")
        else if (characters.nonEmpty) IO.fail(s"Spurious characters: '${characters.get}'")
        else IO.succeed(Empty)

      case ContentType.Text =>
        if (elements.nonEmpty) IO.fail(s"Spurious elements: $elements")
        else IO.succeed(Characters(characters))

      case ContentType.Elements =>
        if (characters.nonEmpty) IO.fail(s"Spurious characters: '${characters.get}'")
        else IO.succeed(Elements(0, elements))

      case ContentType.Mixed =>
        IO.succeed(Mixed(0, nodes))
    }
  }

  val takeCharacters: Modifier[Option[String]] = {
    case Characters(characters) =>
      IO.succeed((Characters(None), characters))

    case Mixed(nextElementNumber, nodes) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      if (elements.nonEmpty) IO.fail(s"Elements in $this")
      else IO.succeed((Mixed(nextElementNumber, Seq.empty), toCharacters(nonElements)))

    case content => IO.fail(s"No characters in $content")
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
    case content@Elements(nextElementNumber, elements) => IO.succeed {
      elements.headOption.fold[(Content, Option[Elem])]((content, None)) { result =>
        (Elements(nextElementNumber + 1, elements.tail), Some(result))
      }
    }

    case content@Mixed(nextElementNumber, nodes) => IO.succeed {
      val noLeadingWhitespace = dropWhitespace(nodes)
      noLeadingWhitespace.headOption.fold[(Content, Option[Elem])]((content, None)) {
        case result: Elem => (Mixed(nextElementNumber + 1, noLeadingWhitespace.tail), Some(result))
        case _ => (content, None)
      }
    }

    case content => IO.fail(s"No element in $content")
  }

  val takeAllNodes: Modifier[Seq[Node]] = {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      IO.succeed(Elements(nextElementNumber, Seq.empty), elements)

    case Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      IO.succeed(Mixed(nextElementNumber, Seq.empty), nodes)

    case content => IO.fail(s"No nodes in $content")
  }

  val takeAllElements: Modifier[Seq[Elem]] = {
    case Elements(nextElementNumber: Int, elements: Seq[Elem]) =>
      IO.succeed(Elements(nextElementNumber, Seq.empty), elements)

    case content@Mixed(nextElementNumber: Int, nodes: Seq[Node]) =>
      val (elements: Seq[Elem], nonElements: Seq[Node]) = partition(nodes)
      val hasNonWhitespace: Boolean = nonElements.exists(node => !isWhitespace(node))
      if (hasNonWhitespace) IO.fail(s"Non white-space nodes in $content")
      else IO.succeed((Mixed(nextElementNumber, Seq.empty), elements))

    case content => IO.fail(s"No elements in $content")
  }

  val checkNoLeftovers: Content => IO[Error, Unit] = {
    case Empty => Parser.ok

    case Characters(characters) =>
      characters.fold[IO[Error, Unit]](Parser.ok)(characters => IO.fail(s"Unparsed characters: $characters"))

    case Elements(_, elements) =>
      if (elements.isEmpty) Parser.ok else IO.fail(s"Unparsed elements: $elements")

    case Mixed(_, nodes) =>
      if (nodes.isEmpty) Parser.ok else IO.fail(s"Unparsed nodes: $nodes")
  }

  private def partition(nodes: Seq[Node]): (Seq[Elem], Seq[Node]) = {
    val (elems, nonElems) = nodes.partition(_.isInstanceOf[Elem])
    (elems.map(_.asInstanceOf[Elem]), nonElems)
  }

  private def toCharacters(nodes: Seq[Node]): Option[String] = {
    val result = nodes.map(_.text).mkString.trim
    if (result.isEmpty) None else Some(result)
  }

  private def dropWhitespace(nodes: Seq[Node]): Seq[Node] =
    nodes.dropWhile(isWhitespace)

  private def isWhitespace(node: Node): Boolean =
    node.text.trim.isEmpty
}
