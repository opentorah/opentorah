package org.opentorah.xml

import scala.xml.{Elem, Node}
import zio.IO

private[xml] sealed trait Content {
  def takeNextElement(p: Elem => Boolean): Content.Next[Option[Elem]]

  def takeAllNodes: Content.Next[Seq[Node]]

  def takeCharacters: Content.Next[Option[String]]

  def checkNoLeftovers: Result
}

private[xml] object Content {

  type Next[A] = IO[Error, (Content, A)]

  private final case object Empty extends Content {
    override def takeNextElement(p: Elem => Boolean): Next[Option[Elem]] =
      IO.fail(s"No element in $this")

    override def takeAllNodes: Next[Seq[Node]] =
      IO.fail(s"No nodes in $this")

    override def takeCharacters: Next[Option[String]] =
      IO.fail(s"No characters in $this")

    override def checkNoLeftovers: Result = ok
  }

  private final case class Characters(characters: Option[String]) extends Content {
    override def takeNextElement(p: Elem => Boolean): Next[Option[Elem]] =
      IO.fail(s"No element in $this")

    override def takeAllNodes: Next[Seq[Node]] =
      IO.fail(s"No nodes in $this")

    override def takeCharacters: Next[Option[String]] =
      IO.succeed((copy(characters = None), characters))

    override def checkNoLeftovers: Result =
      characters.fold[Result](ok)(characters => IO.fail(s"Unparsed characters: $characters"))
  }

  private abstract sealed class HasElements(nextElementNumber: Int, nodes: Seq[Node]) extends Content {
    protected final def nextElement(p: Elem => Boolean): (Int, Seq[Node], Option[Elem]) = {
      val noLeadingWhitespace = nodes.dropWhile(Xml.isWhitespace)
      val (result, newNodes) = noLeadingWhitespace.headOption.fold[(Option[Elem], Seq[Node])]((None, nodes)) {
        case result: Elem if p(result) => (Some(result), noLeadingWhitespace.tail)
        case _ => (None, nodes)
      }
      (nextElementNumber + (if (result.isEmpty) 0 else 1), newNodes, result)
    }

    override def checkNoLeftovers: Result =
      if (nodes.forall(Xml.isWhitespace)) ok else IO.fail(s"Unparsed nodes: $nodes")
  }

  private final case class Elements(nextElementNumber: Int, nodes: Seq[Node]) extends HasElements(nextElementNumber, nodes) {
    override def takeNextElement(p: Elem => Boolean): Next[Option[Elem]] = IO.succeed {
      val (newNextElementNumber: Int, newNodes: Seq[Node], result: Option[Elem]) = nextElement(p)
      (copy(nextElementNumber = newNextElementNumber, nodes = newNodes), result)
    }

    override def takeAllNodes: Next[Seq[Node]] = IO.succeed(copy(nodes = Seq.empty), nodes)

    override def takeCharacters: Next[Option[String]] =
      IO.fail(s"No characters in $this")
  }

  private final case class Mixed(nextElementNumber: Int, nodes: Seq[Node]) extends HasElements(nextElementNumber, nodes) {
    override def takeNextElement(p: Elem => Boolean): Next[Option[Elem]] = IO.succeed {
      val (newNextElementNumber: Int, newNodes: Seq[Node], result: Option[Elem]) = nextElement(p)
      (copy(nextElementNumber = newNextElementNumber, nodes = newNodes), result)
    }

    override def takeAllNodes: Next[Seq[Node]] = IO.succeed(copy(nodes = Seq.empty), nodes)

    override def takeCharacters: Next[Option[String]] = {
      val (elements: Seq[Elem], characters: Option[String]) = partition(nodes)
      if (elements.nonEmpty) IO.fail(s"Elements in $this")
      else IO.succeed((copy(nodes = Seq.empty), characters))
    }
  }

  def open(nodes: Seq[Node], contentType: ContentType): IO[Error, Content] = {
    val (elements: Seq[Elem], characters: Option[String]) = partition(nodes)

    contentType match {
      case ContentType.Empty =>
        if (elements.nonEmpty) IO.fail(s"Spurious elements: $elements")
        else if (characters.nonEmpty) IO.fail(s"Spurious characters: '${characters.get}'")
        else IO.succeed(Empty)

      case ContentType.Characters =>
        if (elements.nonEmpty) IO.fail(s"Spurious elements: $elements")
        else IO.succeed(Characters(characters))

      case ContentType.Elements =>
        if (characters.nonEmpty) IO.fail(s"Spurious characters: '${characters.get}'")
        else IO.succeed(Elements(0, nodes))

      case ContentType.Mixed =>
        IO.succeed(Mixed(0, nodes))
    }
  }

  private def partition(nodes: Seq[Node]): (Seq[Elem], Option[String]) = {
    val (elems, nonElems) = nodes.partition(Xml.isElement)
    val characters = nonElems.map(_.text).mkString.trim
    (elems.map(Xml.asElement), if (characters.isEmpty) None else Some(characters))
  }
}
