package org.digitaljudaica.metadata

import cats.data.StateT
import cats.implicits._
import scala.xml.{Elem, Node}

object XmlParser {

  final case class Element(
    name: String,
    attributes: Map[String, String],
    elements: Seq[Elem],
    characters: Option[String]
  )

  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type Parser[A] = StateT[ErrorOr, Element, A]

  private def pure[A](value: A): Parser[A] = StateT.pure[ErrorOr, Element, A](value)

  private def lift[A](value: ErrorOr[A]): Parser[A] = StateT.liftF[ErrorOr, Element, A](value)

  private def inspect[A](f: Element => A): Parser[A] = StateT.inspect[ErrorOr, Element, A](f)

  private def modify(f: Element => Element): Parser[Unit] = StateT.modify[ErrorOr, Element](f)

  def check(condition: Boolean, message: => String): Parser[Unit] =
    lift(if (condition) Right(()) else Left(message))

  private val getName: Parser[String] = inspect(_.name)

  private val getElements: Parser[Seq[Elem]] = inspect(_.elements)

  private val getCharacters: Parser[Option[String]] = inspect(_.characters)

  def optionalAttribute(name: String): Parser[Option[String]] = for {
    result <- inspect(_.attributes.get(name))
    _ <- modify(element => element.copy(attributes = element.attributes - name))
  } yield result

  def attribute(name: String): Parser[String] = for {
    resultO <- optionalAttribute(name)
    _ <- check(resultO.isDefined, s"Required attribute '$name' is missing")
    result = resultO.get
  } yield result

  def optionalBooleanAttribute(name: String): Parser[Option[Boolean]] = for {
    resultO <- optionalAttribute(name)
    result = resultO.map(value => value == "true" || value == "yes")
  } yield result

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

  def element[A](name: String, parser: Parser[A]): Parser[A] = for {
    resultO <- optionalElement(name, parser)
    _ <- check(resultO.isDefined, s"Required element '$name' is missing")
    result = resultO.get
  } yield result

  def elements[A](name: String, parser: Parser[A]): Parser[Seq[A]] = for {
    elements <- getElements
    (toParse, tail) = elements.span(_.label == name)
    _ <- modify(_.copy(elements = tail))
    result <- toParse.toList.traverse(xml => lift(run(xml, name, parser)))
  } yield result

  def run[A](elem: Elem, name: String, parser: Parser[A]): ErrorOr[A] = {
    val result = for {
      // check element name
      elementName <- getName
      _ <- check(elementName == name, s"Wrong element: $elementName instead of $name")

      // no mixed content
      elements <- getElements
      characters <- getCharacters
      _ <- check(elements.isEmpty || characters.isEmpty, s"Mixed content: $elem")

      // parse
      result <- parser

      // no left-over elements
      elementsAfter <- getElements
      _ <- check(elementsAfter.isEmpty, s"Unparsed elements: $elementsAfter")

      // no left-over characters
      charactersAfter <- getCharacters
      _ <- check(charactersAfter.isEmpty, s"Unparsed characters: ${charactersAfter.get}")
    } yield result

    result.runA {
      val (elements: Seq[Node], nonElements: Seq[Node]) = elem.child.partition(_.isInstanceOf[Elem])
      Element(
        name = elem.label,
        attributes = elem.attributes.map { metadata => metadata.key -> metadata.value.toString }.toMap,
        elements = elements.map(_.asInstanceOf[Elem]),
        characters = if (nonElements.isEmpty) None else Some((nonElements map (_.text)).mkString)
      )
    }
  }

  def runA[A](elem: Elem, name: String, parser: Parser[A]): A =
    run(elem, name, parser).fold(error => throw new IllegalArgumentException(error), identity)

  val languageSpecParser: Parser[LanguageSpec] = for {
    lang <- optionalAttribute("lang")
    isTransliterated <- optionalBooleanAttribute("transliterated")
    flavour <- optionalAttribute("flavour")
  } yield LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = isTransliterated,
    flavour = flavour
  )

  val nameParser: Parser[Name] = for {
    n <- optionalAttribute("n")
    characters <- optionalCharacters
    _ <- check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
    _ <- check(n.isEmpty || characters.isEmpty, "Both 'n' attribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- languageSpecParser
  } yield Name(name.get, languageSpec)
}
