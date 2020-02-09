package org.digitaljudaica.metadata

import cats.implicits._
import cats.data.StateT
import org.digitaljudaica.xml.Element
import scala.xml.{Elem, Node, Text}

object Xml {

  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type Parser[A] = StateT[ErrorOr, Element, A]

  def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Element, A](value)

  private def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Element, A](value)

  private def inspect[A](f: Element => A): Parser[A] = StateT.inspect[ErrorOr, Element, A](f)

  private def modify(f: Element => Element): Parser[Unit] = StateT.modify[ErrorOr, Element](f)

  def check(condition: Boolean, message: => String): Parser[Unit] =
    lift(if (condition) Right(()) else Left(message))

  private val getName: Parser[String] = inspect(_.name)

  private val getElements: Parser[Seq[Elem]] = inspect(_.elements)

  private val getCharacters: Parser[Option[String]] = inspect(_.characters)

  private def required[A](what: String, name: String, parser: String => Parser[Option[A]]): Parser[A] = for {
    result <- parser(name)
    _ <- check(result.isDefined, s"Required $what '$name' is missing")
  } yield result.get

  def optionalAttribute(name: String): Parser[Option[String]] = for {
    result <- inspect(_.attributes.get(name))
    _ <- modify(element => element.copy(attributes = element.attributes - name))
  } yield result

  def attribute(name: String): Parser[String] =
    required(s"attribute", name, optionalAttribute)

  def optionalBooleanAttribute(name: String): Parser[Option[Boolean]] = for {
    resultO <- optionalAttribute(name)
    result = resultO.map(value => value == "true" || value == "yes")
  } yield result

  def optionalIntAttribute(name: String): Parser[Option[Int]] = for {
    resultO <- optionalAttribute(name)
    result = resultO.map(_.toInt) // TODO turn exception into our error
    _ <- check(result.isEmpty || result.get > 0, s"Non-positive integer: ${result.get}")
  } yield result

  def intAttribute(name: String): Parser[Int] =
    required(s"attribute", name, optionalIntAttribute)

  def optionalCharacters: Parser[Option[String]] = for {
    result <- getCharacters
    _ <- modify(_.copy(characters = None))
  } yield result

  def optionalElement[A](name: String, parser: Parser[A]): Parser[Option[A]] = for {
    elements <- getElements
    toParse = elements.headOption.filter(_.label == name)
    result <- if (toParse.isEmpty) pure(None) else lift(run(toParse.get, name, parser).map(Some(_)))
    _ <- modify(if (toParse.isEmpty) identity else _.copy(elements = elements.tail))
  } yield result

  def element[A](toParse: Elem, name: String, parser: Parser[A]): Parser[A] =
    lift(run(toParse, name, parser))

  def element[A](name: String, parser: Parser[A]): Parser[A] =
    required(s"element", name, optionalElement(_, parser))

  def getElements[A](name: String): Parser[Seq[Elem]] = for {
    elements <- getElements
    (result, tail) = elements.span(_.label == name)
    _ <- modify(_.copy(elements = tail))
  } yield result

  def elements[A](toParse: Seq[Elem], name: String, parser: Parser[A]): Parser[Seq[A]] = for {
    result <- toParse.toList.traverse(xml => lift(run(xml, name, parser)))
  } yield result

  def elements[A](name: String, parser: Parser[A]): Parser[Seq[A]] = for {
    toParse <- getElements(name)
    result <- elements(toParse, name, parser)
  } yield result

  private def run[A](elem: Elem, name: String, parser: Parser[A]): ErrorOr[A] = {
    val result = for {
      // check element name
      elementName <- getName
      _ <- check(elementName == name, s"Wrong element: $elementName instead of $name")

      // no mixed content
      elements <- getElements
      characters <- getCharacters
      _ <- check(elements.isEmpty || characters.isEmpty, s"Mixed content: [${characters.get}] $elem")

      // parse
      result <- parser

      // no left-over elements
      elementsAfter <- getElements
      _ <- check(elementsAfter.isEmpty, s"Unparsed elements: $elementsAfter")

      // no left-over characters
      charactersAfter <- getCharacters
      _ <- check(charactersAfter.isEmpty, s"Unparsed characters: ${charactersAfter.get}")
    } yield result

    result.runA(Element(url = None, elem))
  }

  def runA[A](elem: Elem, name: String, parser: Parser[A]): A =
    run(elem, name, parser).fold(error => throw new IllegalArgumentException(error), identity)



  // TODO switch to Parser[A]
  def open(element: Elem, name: String): (Attributes, Seq[Elem]) = {
    checkName(element, name)
    checkNoNonElements(element)
    (Attributes(element), getElements(element))
  }

  // TODO switch to Parser[A]
  def openEmpty(element: Elem, name: String): Attributes = {
    checkName(element, name)
    checkNoElements(element)
    checkNoNonElements(element)
    Attributes(element)
  }

  // TODO switch to Parser[A]
  def openText(element: Elem, name: String): (Attributes, Option[String]) = {
    checkName(element, name)
    checkNoElements(element)
    val text = element.text
    (Attributes(element), if (text.isEmpty) None else Some(text))
  }

  // TODO switch to Parser[A]
  private def checkName(element: Elem, name: String): Unit =
    require(element.label == name, s"Wrong element: ${element.label} instead of $name")

  // TODO switch to Parser[A]
  private def checkNoElements(element: Elem): Unit =
    require(getElements(element).isEmpty, "Nested elements present.")

  // TODO switch to Parser[A]
  private def checkNoNonElements(element: Elem): Unit = {
    val nonElements = getNonElements(element)
    require(nonElements.isEmpty, s"Non-element children present on element $element: $nonElements")
  }

  // TODO switch to Parser[A]
  private def getElements(element: Elem): Seq[Elem] =
    element.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  // TODO switch to Parser[A]
  private def getNonElements(element: Elem): Seq[Node] = {
    element.child.filterNot(_.isInstanceOf[Elem]).filter { node =>
      !node.isInstanceOf[Text] ||
        node.asInstanceOf[Text].text.replace('\n', ' ').trim.nonEmpty
    }
  }

  // TODO switch to Parser[A]
  def take(elements: Seq[Elem], name1: String): (Seq[Elem], Seq[Elem]) = {
    elements.span(_.label == name1)
  }

  // TODO switch to Parser[A]
  def parseEmpty[T](element: Elem, name: String, parser: Attributes => T): T = {
    val attributes = openEmpty(element, name)
    val result = parser(attributes)
    attributes.close()
    result
  }

  // TODO switch to Parser[A]
  def noMoreThanOne(elements: Seq[Elem]): Option[Elem] = {
    require(elements.length <= 1)
    elements.headOption
  }

  // TODO switch to Parser[A]
  def span(elements: Seq[Elem], name1: String): Seq[Elem] = {
    val (result, tail) = take(elements, name1)
    checkNoMoreElements(tail)
    result
  }

  // TODO switch to Parser[A]
  def span(elements: Seq[Elem], name1: String, name2: String): (Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    checkNoMoreElements(tail2)
    (elements1, elements2)
  }

  // TODO switch to Parser[A]
  def span(elements: Seq[Elem], name1: String, name2: String, name3: String): (Seq[Elem], Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    val (elements3, tail3) = tail2.span(_.label == name3)
    checkNoMoreElements(tail3)
    (elements1, elements2, elements3)
  }

  // TODO switch to Parser[A]
  def checkNoMoreElements(elements: Seq[Elem]): Unit =
    require(elements.isEmpty, s"Spurious elements: ${elements.head.label}")
}
