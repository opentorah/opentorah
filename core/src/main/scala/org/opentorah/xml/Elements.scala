package org.opentorah.xml

import org.opentorah.util.{Collections, Effects}
import zio.ZIO

trait Elements[A]:

  def elementAndParser(name: String): Option[Element.AndParser[A]]

  protected def elementByValue(value: A): Element[?]

  def xmlElement(value: A): Xml.Element =
    elementByValue(value).asInstanceOf[Element[A]].xmlElement(value)

  private def parseOption: Parser[Option[A]] = ParserState.optional(this)

  final def parse(from: From): Parser[A] = ParserState.required(this, from)

  final def optional: Elements.Optional[A] = Elements.Optional[A](this)
  final def required: Elements.Required[A] = Elements.Required[A](this)
  final def seq     : Elements.Sequence[A] = Elements.Sequence[A](this)

  // Note: this is only used in Named and ListFile, but still...
  final def wrappedSeq(wrapperElementName: String): Element[Seq[A]] = new Element[Seq[A]](wrapperElementName):
    override def contentParsable: Parsable[Seq[A]] = Elements.this.seq

object Elements:

  sealed abstract class Parsable[A, C] extends org.opentorah.xml.Parsable[A]:
    def xml(value: A): C

  final class Optional[A](elements: Elements[A]) extends Parsable[Option[A], Option[Xml.Element]]:
    override protected def parser: Parser[Option[A]] = elements.parseOption
    override def xml(value: Option[A]): Option[Xml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Option[A]] = Unparser[Option[A]](
      content = value => xml(value).toSeq
    )

  final class Required[A](elements: Elements[A]) extends Parsable[A, Xml.Element]:
    override protected def parser: Parser[A] = Effects.required(elements.parseOption, elements)
    override def xml(value: A): Xml.Element = elements.xmlElement(value)
    override def unparser: Unparser[A] = Unparser[A](
      content = value => Seq(xml(value))
    )

  final class Sequence[A](elements: Elements[A]) extends Parsable[Seq[A], Seq[Xml.Element]]:
    override protected def parser: Parser[Seq[A]] = all(Seq.empty)
    private def all(acc: Seq[A]): Parser[Seq[A]] = for
      next: Option[A] <- elements.parseOption
      result: Seq[A] <- next
        .map(next => all(acc :+ next))
        .getOrElse(ZIO.succeed(acc))
    yield result
    override def xml(value: Seq[A]): Seq[Xml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Seq[A]] = Unparser[Seq[A]](
      content = value => xml(value)
    )

  abstract class Union[A] extends Elements[A]:
    protected def elements: Seq[Element[? <: A]]

    final override def elementAndParser(name: String): Option[Element.AndParser[A]] =
      elements.find(_.elementName == name).map((element: Element[? <: A]) => Element.AndParser[A](
        element = element,
        parser = element.contentParsable()
      ))

  final class Choices(result: Map[Element[?], Seq[?]]):

    def all[A](element: Element[A]): Seq[A] =
      result.get(element).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)

    def optional[A](element: Element[A]): Parser[Option[A]] =
      val results = all(element)
      for
        _ <- Effects.check(results.length <= 1, s"Too many values for $element")
      yield results.headOption

  private final class Choice(elements: Seq[Element[?]]) extends Elements[(Element[?], ?)]:
    override def elementAndParser(name: String): Option[Element.AndParser[(Element[?], ?)]] =
      elements.find(_.elementName == name).map((element: Element[?]) => Element.AndParser(
        element = element,
        parser = element.contentParsable().map(result => element -> result)
      ))

    override protected def elementByValue(value: (Element[?], ?)): Element[?] =
      throw UnsupportedOperationException("Not supposed to happen!")

  def choices(elements: Seq[Element[?]]): Parser[Choices] = Choice(elements)
    .seq()
    .map((results: Seq[(Element[?], ?)]) => Collections.mapValues(results.groupBy(_._1))(_.map(_._2)))
    .map(Choices(_))
