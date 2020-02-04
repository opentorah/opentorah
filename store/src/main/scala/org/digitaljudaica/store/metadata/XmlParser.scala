package org.digitaljudaica.store.metadata

import cats.data.State
import scala.xml.{Elem, Node}
object XmlParser {

  // TODO
  // distinguish between warnings (that can be accumulated) and errors (that stop parsing - but not with an exception);
  // mix in Error monad - or use ZIO?

  trait Content
  final case class Elements(elements: Seq[Elem]) extends Content
  final case class Characters(characters: String) extends Content

  final case class Input(
    attributes: Map[String, String] = Map.empty,
    content: Content = Elements(Seq.empty),
    errors: Seq[String] = Seq.empty
  ) {
    def addError(message: String): Input = addErrors(Seq(message))
    def addError(condition: Boolean, message: String): Input = if (!condition) this else addErrors(Seq(message))
    def addErrors(messages: Seq[String]): Input = this.copy(errors = errors ++ messages)

    // TODO rework as parsers and is in a for
    def getElements: (Input, Seq[Elem]) = content match {
      case Characters(_) =>
        (addError("Character content where element is expected"), Seq.empty)
      case Elements(elements) => (this, elements)
    }

    // TODO rework as parsers and is in a for
    def getCharacters: (Input, Option[String]) = content match {
      case Characters(characters) => (this.withElements(Seq.empty), Some(characters))
      case Elements(elements) =>
        (addError(elements.nonEmpty, s"Element content where characters are expected: $elements"), None)
    }

    def withElements(elements: Seq[Elem]): Input = copy(content = Elements(elements))

    def close: Input = {
      val result = content match {
        case Elements(elements) => addError(elements.nonEmpty, s"Unparsed elements: $elements")
        case Characters(characters) => addError(s"Unparsed characters: $characters")
      }
      result.addError(attributes.nonEmpty, "Unparsed attributes: " + attributes.keySet.mkString(", "))
    }
  }

  type Parser[A] = State[Input, A]

  def open(xml: Elem, name: String): Input = {
    if (xml.label != name) Input().addErrors(Seq(s"Wrong element: ${xml.label} instead of $name")) else {
      val (elements: Seq[Node], nonElements: Seq[Node]) = xml.child.partition(_.isInstanceOf[Elem])
      Input(
        attributes = xml.attributes.map { metadata => metadata.key -> metadata.value.toString }.toMap,
        content = if (nonElements.nonEmpty) Characters(xml.text) else Elements(elements.map(_.asInstanceOf[Elem])),
      ).addError(elements.nonEmpty && nonElements.nonEmpty, s"Mixed content: $xml")
    }
  }

  def addError(condition: Boolean, message: String): Parser[Unit] =
    State { input: Input => (input.addError(condition, message), ()) }

  def optionalAttribute(name: String): Parser[Option[String]] =
    State { input: Input => (input.copy(attributes = input.attributes - name), input.attributes.get(name)) }

  def attribute(name: String): Parser[String] = for {
    result <- optionalAttribute(name)
  } yield {
    if (result.isEmpty) throw new IllegalArgumentException(s"Required attribute '$name' is missing")
    result.get
  }

  def getBoolean(value: Option[String]): Option[Boolean] = value.map(value => value == "true" || value == "yes")

  val optionalCharacters: Parser[Option[String]] = State { input: Input => input.getCharacters }

  def optionalElement[A](name: String, parser: Parser[A]): Parser[Option[A]] = State { input: Input =>
    val (output, elements) = input.getElements
    val element = elements.headOption.filter(_.label == name)
    if (element.isEmpty) (output, None) else {
      val (errors, result) = run(element.get, name, parser)
      (output.withElements(elements.tail).addErrors(errors), Some(result))
    }
  }

  def element[A](name: String, parser: Parser[A]): Parser[A] = for {
    result <- optionalElement(name, parser)
  } yield {
    if (result.isEmpty) throw new IllegalArgumentException(s"Required element '$name' is missing")
    result.get
  }

  def elements[A](name: String, parser: Parser[A]): Parser[Seq[A]] = State { input: Input =>
    val (output, elements) = input.getElements
    val (toParse: Seq[Elem], tail: Seq[Elem]) = elements.span(_.label == name)
    val result: Seq[(Seq[String], A)] = toParse.map { xml => run(xml, name, parser) }
    (output.withElements(tail).addErrors(result.flatMap(_._1)), result.map(_._2))
  }

  def run[A](xml: Elem, name: String, parser: Parser[A]): (Seq[String], A) = {
    val (input, result) = parser.run(open(xml, name)).value
    (input.close.errors, result)
  }

  def runToFinish[A](xml: Elem, name: String, parser: Parser[A]): A = {
    val (errors, result) = run(xml, name, parser)
    if (errors.nonEmpty) throw new IllegalArgumentException(s"Parsing errors: " + errors.mkString(", "))
    result
  }
}
