package org.digitaljudaica.metadata

import java.io.{File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter, Writer}
import cats.implicits._
import cats.data.StateT
import org.digitaljudaica.xml.Element
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, PrettyPrinter, Text, TopScope}

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

  def required[A](what: String, name: String, parser: String => Parser[Option[A]]): Parser[A] = for {
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


  private def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  private def removeNamespace(element: Elem): Elem =
    element.copy(scope = TopScope, child = element.child.map(removeNamespace))

  def spacedText(node: Node): String = node match {
    case elem: Elem => (elem.child map (_.text)).mkString(" ")
    case node: Node => node.text
  }

  def rewriteElements(xml: Elem, elementRewriter: Elem => Elem): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => elementRewriter(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }

  // --- Pretty printing:
  private val width = 120

  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(width, 2) {
    // TODO: it seems that there is a bug in PrettyPrinter, but with this override uncommented stack overflows...
//    override protected def makeBox(ind: Int, s: String): Unit =
//      if (cur + s.length <= width) { // fits in this line; LMD: changed > to <=...
//        items ::= Box(ind, s)
//        cur += s.length
//      } else try cut(s, ind) foreach (items ::= _) // break it up
//      catch { case _: BrokenException => makePara(ind, s) } // give up, para
  }

  // TODO PrettyPrinter breaks the line between e1 and e2 in <e1>...</e1><e2>...</e2>
  // and between e1 and text in: <e1>...<e1>text;
  // should I try fixing that?
  // or implement my own based on http://www.lihaoyi.com/post/CompactStreamingPrettyPrintingofHierarchicalData.html ?
  // or move to DOM and use org.apache.xml.serializer.dom3.LSSerializerImpl?
  // or move via DOM to ScalaTags (implementation "com.lihaoyi:scalatags_$scalaVersionMajor:$scalaTagsVersion")?
  // or use `spotless` with the Eclipse formatter?
  private val join: Set[String] = Set(".", ",", ";", ":", "\"", ")")

  def format(elem: Elem): String = {
    @scala.annotation.tailrec
    def merge(result: List[String], lines: List[String]): List[String] = lines match {
      case l1 :: l2 :: ls =>
        val l = l2.trim
        if (join.exists(l.startsWith))
          merge(result, (l1 ++ l) :: ls)
        else
          merge(result :+ l1, l2 :: ls)
      case l :: Nil => result :+ l
      case Nil => result
    }

    val result: String = prettyPrinter.format(elem)

    // pretty-printer splits punctuation from the preceding elements; merge them back :)
    merge(List.empty, result.split("\n").toList).mkString("\n")
  }

  def print(xml: Node, outStream: OutputStream): Unit = print(xml, new OutputStreamWriter(outStream))
  def print(xml: Node, outFile: File): Unit = print(xml, new FileWriter(outFile))

  def print(xml: Node, writer: Writer): Unit = {
    val out = new PrintWriter(writer)
    val pretty = prettyPrinter.format(xml)
    // TODO when outputting XML, include <xml> header?
    out.println(pretty)
    out.close()
  }

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



  implicit class Ops(elem: Elem) {

    def elemsFilter(name: String): Seq[Elem] = elem.elems.filter(_.label == name)

    // TODO dup!
    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }

    def descendants(name: String): Seq[Elem] = elem.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def getAttribute(name: String): String = attributeOption(name).getOrElse(throw new NoSuchElementException(s"No attribute $name"))

    // TODO difference?
    def attributeOption(name: String): Option[String] = elem.attributes.asAttrMap.get(name)
//    def attributeOption(name: String): Option[String] = {
//      val result: Seq[Node] = elem \ ("@" + name)
//      if (result.isEmpty) None else Some(result.text)
//    }

    def idOption: Option[String] = attributeOption("xml:id")

    def id: String = getAttribute("xml:id")

    def oneChild(name: String): Elem = oneOptionalChild(name, required = true).get
    def optionalChild(name: String): Option[Elem] = oneOptionalChild(name, required = false)

    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      if (children.size > 1) throw new NoSuchElementException(s"To many children with name '$name'")
      if (required && children.isEmpty) throw new NoSuchElementException(s"No child with name '$name'")

      if (children.isEmpty) None else Some(children.head.asInstanceOf[Elem])
    }

    def check(name: String): Elem = {
      if (elem.label != name) throw new NoSuchElementException(s"Expected name $name but got $elem.label")
      elem
    }

    def withoutNamespace: Elem = removeNamespace(elem)

    def format: String = Xml.format(elem)
  }
}
