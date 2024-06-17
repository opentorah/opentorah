package org.opentorah.xml

import org.opentorah.util.{Collections, Effects}
import zio.ZIO

trait ElementsTo[A]:
  type ElementType

  protected def elements: Seq[ElementTo[? <: ElementType]]

  final def elementByName(name: String): Option[ElementTo[? <: ElementType]] = elements.find(_.elementName == name)

  def map(elementTo: ElementTo[? <: ElementType], parser: Parser[ElementType]): Parser[A]

  protected def elementByValue(value: A): ElementTo[? <: ElementType]

  private final def xmlElement(value: A): Element = elementByValue(value).asInstanceOf[ElementTo[A]].xmlElement(value) // TODO remove cast

  private def parseOption: Parser[Option[A]] = ParserState.accessZIO(_.optional(this))

  final def parse(from: From): Parser[A] = ParserState.accessZIO(_.required(this, from))

  final def optional: ElementsTo.Optional[A] = ElementsTo.Optional[A](this)
  final def required: ElementsTo.Required[A] = ElementsTo.Required[A](this)
  final def seq     : ElementsTo.Sequence[A] = ElementsTo.Sequence[A](this)

  // Note: this is only used in Named and ListFile, but still...
  final def wrappedSeq(wrapperElementName: String): ElementTo[Seq[A]] = new ElementTo[Seq[A]](wrapperElementName):
    override def contentParsable: Parsable[Seq[A]] = ElementsTo.this.seq

  def descendants(nodes: Nodes, elementName: String): Parser[Seq[A]] = ZIO.foreach(
    Nodes.descendants(nodes, elementName).filter(Element.is).map(Element.as)
  )(descendant => parse(From.xml("descendants", descendant)))
  
object ElementsTo:
  sealed abstract class Parsable[A, C] extends org.opentorah.xml.Parsable[A]:
    def xml(value: A): C

  final class Optional[A](elementsTo: ElementsTo[A]) extends Parsable[Option[A], Option[Element]]:
    override protected def parser: Parser[Option[A]] = elementsTo.parseOption
    override def xml(value: Option[A]): Option[Element] = value.map(elementsTo.xmlElement)
    override def unparser: Unparser[Option[A]] = Unparser[Option[A]](content = value => xml(value).toSeq)

  final class Required[A](elementsTo: ElementsTo[A]) extends Parsable[A, Element]:
    override protected def parser: Parser[A] = Effects.required(elementsTo.parseOption, elementsTo)
    override def xml(value: A): Element = elementsTo.xmlElement(value)
    override def unparser: Unparser[A] = Unparser[A](content = value => Seq(xml(value)))

  final class Sequence[A](elementsTo: ElementsTo[A]) extends Parsable[Seq[A], Elements]:
    override protected def parser: Parser[Seq[A]] = all(Seq.empty)
    private def all(acc: Seq[A]): Parser[Seq[A]] = for
      next: Option[A] <- elementsTo.parseOption
      result: Seq[A] <- next.map((next: A) => all(acc :+ next)).getOrElse(ZIO.succeed(acc))
    yield result
    override def xml(value: Seq[A]): Elements = value.map(elementsTo.xmlElement)
    override def unparser: Unparser[Seq[A]] = Unparser[Seq[A]](content = value => xml(value))

  abstract class Union[A] extends ElementsTo[A]:
    final def xmlElement(value: A): Element = super.xmlElement(value)
    final override type ElementType = A
    final override def map(elementTo: ElementTo[? <: A], parser: Parser[A]): Parser[A] = parser

  final class Choices(result: Map[ElementTo[?], Seq[?]]):
    def apply[A](elementTo: ElementTo[A]): Parser[Option[A]] =
      val results: Seq[A] = result.get(elementTo).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)
      for _ <- Effects.check(results.length <= 1, s"Too many values for $elementTo") yield results.headOption

  private final class Choice(
    override protected val elements: Seq[ElementTo[?]]
  ) extends ElementsTo[(ElementTo[?], ?)]:
    override type ElementType = Any

    override def map(elementTo: ElementTo[?], parser: Parser[Any]): Parser[(ElementTo[?], ?)] =
      parser.map(result => elementTo -> result)

    override protected def elementByValue(value: (ElementTo[?], ?)): ElementTo[?] =
      throw UnsupportedOperationException("Not supposed to happen!")

  def choices(elements: ElementTo[?]*): Parser[Choices] = Choice(elements)
    .seq()
    .map((results: Seq[(ElementTo[?], ?)]) => Collections.mapValues(results.groupBy(_._1))(_.map(_._2)))
    .map(Choices(_))
