package org.digitaljudaica.xml

import scala.xml.{Elem, Node}

private[xml] sealed trait Content

private[xml] object Content {

  sealed trait Type

  object Type {
    final case object Empty extends Type
    final case object Characters extends Type
    final case object Elements extends Type
    final case object Mixed extends Type
  }

  private final case object Empty extends Content

  private final case class Characters(characters: Option[String]) extends Content

  private final case class Elements(nextElementNumber: Int, elements: Seq[Elem]) extends Content

  private final case class Mixed(nextElementNumber: Int, nodes: Seq[Node]) extends Content

  def open(nodes: Seq[Node], contentType: Type): ErrorOr[Content] = {
    val (elements: Seq[Node], nonElements: Seq[Node]) = nodes.partition(_.isInstanceOf[Elem])
    val characters: String = nonElements.map(_.text).mkString.trim

    contentType match {
      case Type.Empty =>
        if (elements.nonEmpty) Left(s"Spurious elements: $elements")
        else if (characters.nonEmpty) Left(s"Spurious characters: '$characters'")
        else Right(Empty)

      case Type.Characters =>
        if (elements.nonEmpty) Left(s"Spurious elements: $elements")
        else Right(Characters(if (characters.isEmpty) None else Some(characters)))

      case Type.Elements =>
        if (characters.nonEmpty) Left(s"Spurious characters: '$characters'")
        else Right(Elements(0, elements.map(_.asInstanceOf[Elem])))

      case Type.Mixed =>
        Right(Mixed(0, nodes))
    }
  }

  def takeCharacters(content: Content): ErrorOr[(Content, Option[String])] = content match {
    case Characters(characters) => Right((Characters(None), characters))
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
      val (elements: Seq[Node], nonElements: Seq[Node]) = nodes.partition(_.isInstanceOf[Elem])
      val hasNonWhitespace: Boolean = nonElements.exists(node => !isWhitespace(node))
      if (hasNonWhitespace) Left(s"Non white-space nodes in $content")
      else Right((Mixed(nextElementNumber, Seq.empty), elements.map(_.asInstanceOf[Elem])))

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

  private def dropWhitespace(nodes: Seq[Node]): Seq[Node] =
    nodes.dropWhile(isWhitespace)

  private def isWhitespace(node: Node): Boolean =
    node.text.trim.isEmpty
}
