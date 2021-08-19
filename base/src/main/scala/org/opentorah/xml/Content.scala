package org.opentorah.xml

import org.opentorah.util.Effects
import zio.IO

/* TODO all the issues with Scala XML push me towards generalizing parsing
   (and transforms) to work with Dom...
   I should take a look at the following classes:
     Content, Context, Element, Elements, From (and possibly Catalog).
 */
private[xml] sealed trait Content {
  def takeNextElement(p: Xml.Element => Boolean): Content.Next[Option[Xml.Element]]

  def takeAllNodes: Content.Next[Xml.Nodes]

  def takeCharacters: Content.Next[Option[String]]

  def checkNoLeftovers: IO[Effects.Error, Unit]
}

private[xml] object Content {

  type Next[A] = IO[Effects.Error, (Content, A)]

  private case object Empty extends Content {
    override def takeNextElement(p: Xml.Element => Boolean): Next[Option[Xml.Element]] =
      IO.fail(s"No element in $this")

    override def takeAllNodes: Next[Xml.Nodes] =
      IO.fail(s"No nodes in $this")

    override def takeCharacters: Next[Option[String]] =
      IO.fail(s"No characters in $this")

    override def checkNoLeftovers: IO[Effects.Error, Unit] = Effects.ok
  }

  private final case class Characters(characters: Option[String]) extends Content {
    override def takeNextElement(p: Xml.Element => Boolean): Next[Option[Xml.Element]] =
      IO.fail(s"No element in $this")

    override def takeAllNodes: Next[Xml.Nodes] =
      IO.fail(s"No nodes in $this")

    override def takeCharacters: Next[Option[String]] =
      IO.succeed((copy(characters = None), characters))

    override def checkNoLeftovers: IO[Effects.Error, Unit] =
      Effects.check(characters.isEmpty, s"Unparsed characters: ${characters.get}")
  }

  private abstract sealed class HasElements(nextElementNumber: Int, nodes: Xml.Nodes) extends Content {
    protected final def nextElement(p: Xml.Element => Boolean): (Int, Xml.Nodes, Option[Xml.Element]) = {
      val noLeadingWhitespace = nodes.dropWhile(Xml.isWhitespace)
      val (result, newNodes) = noLeadingWhitespace.headOption.fold[(Option[Xml.Element], Xml.Nodes)]((None, nodes)) {
        case result: Xml.Element if p(result) => (Some(result), noLeadingWhitespace.tail)
        case _ => (None, nodes)
      }
      (nextElementNumber + (if (result.isEmpty) 0 else 1), newNodes, result)
    }

    override def checkNoLeftovers: IO[Effects.Error, Unit] =
      Effects.check(nodes.forall(Xml.isWhitespace), s"Unparsed nodes: $nodes")
  }

  private final case class Elements(nextElementNumber: Int, nodes: Xml.Nodes) extends HasElements(nextElementNumber, nodes) {
    override def takeNextElement(p: Xml.Element => Boolean): Next[Option[Xml.Element]] = IO.succeed {
      val (newNextElementNumber: Int, newNodes: Xml.Nodes, result: Option[Xml.Element]) = nextElement(p)
      (copy(nextElementNumber = newNextElementNumber, nodes = newNodes), result)
    }

    override def takeAllNodes: Next[Xml.Nodes] = IO.succeed((copy(nodes = Seq.empty), nodes))

    override def takeCharacters: Next[Option[String]] =
      IO.fail(s"No characters in $this")
  }

  private final case class Mixed(nextElementNumber: Int, nodes: Xml.Nodes) extends HasElements(nextElementNumber, nodes) {
    override def takeNextElement(p: Xml.Element => Boolean): Next[Option[Xml.Element]] = IO.succeed {
      val (newNextElementNumber: Int, newNodes: Xml.Nodes, result: Option[Xml.Element]) = nextElement(p)
      (copy(nextElementNumber = newNextElementNumber, nodes = newNodes), result)
    }

    override def takeAllNodes: Next[Xml.Nodes] = IO.succeed((copy(nodes = Seq.empty), nodes))

    override def takeCharacters: Next[Option[String]] = {
      val (elements: Seq[Xml.Element], characters: Option[String]) = partition(nodes)
      if (elements.nonEmpty) IO.fail(s"Elements in $this")
      else IO.succeed((copy(nodes = Seq.empty), characters))
    }
  }

  def open(nodes: Xml.Nodes, contentType: ContentType): IO[Effects.Error, Content] = {
    val (elements: Seq[Xml.Element], characters: Option[String]) = partition(nodes)

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

  private def partition(nodes: Xml.Nodes): (Seq[Xml.Element], Option[String]) = {
    val (elems, nonElems) = nodes.partition(Xml.isElement)
    val characters = nonElems.map(_.text).mkString.trim
    (elems.map(Xml.asElement), if (characters.isEmpty) None else Some(characters))
  }
}
