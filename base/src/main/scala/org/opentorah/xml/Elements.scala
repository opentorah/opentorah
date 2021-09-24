package org.opentorah.xml

import org.opentorah.util.{Collections, Effects, Files}
import zio.ZIO
import java.net.URL

trait Elements[A]:

  def elementAndParser(name: String): Option[Element.AndParser[A]]

  protected def elementByValue(value: A): Element[?]

  def xmlElement(value: A): ScalaXml.Element =
    elementByValue(value).asInstanceOf[Element[A]].xmlElement(value)

  private def optionalParser: Parser[Option[A]] = Parsing.optional(this)

  final def parse(from: From): Parser[A] = Parsing.required(this, from)
  final def parse(fromUrl: URL, xml: Xml = ScalaXml): Parser[A] = parse(From.url(fromUrl, xml))

  final def optional: Elements.Optional[A] = Elements.Optional[A](this)
  final def required: Elements.Required[A] = Elements.Required[A](this)
  final def seq     : Elements.Sequence[A] = Elements.Sequence[A](this)

  // Note: this is only used in Named and ListFile, but still...
  final def wrappedSeq(wrapperElementName: String): Element[Seq[A]] = new Element[Seq[A]](wrapperElementName):
    override def contentParsable: Parsable[Seq[A]] = Elements.this.seq

  final def followRedirects: Elements.HandleRedirect[A, A] = followRedirects(identity)

  private def followRedirects[B](f: Parser[A] => Parser[B]): Elements.HandleRedirect[A, B] = Elements.HandleRedirect(
    this,
    noRedirect = f,
    redirected = (from: From) => followRedirects(f).parse(from)
  )

  final def orRedirect: Elements.HandleRedirect[A, Elements.Redirect.Or[A]] = Elements.HandleRedirect(
    this,
    noRedirect = _.map(Right(_)),
    redirected = (from: From) => ZIO.left(Elements.Redirect[A](from, this))
  )

  final def withRedirect(follow: Boolean): Elements.HandleRedirect[A, Elements.Redirect.Or[A]] =
    if !follow then orRedirect else followRedirects(_.map(Right(_)))

object Elements:

  sealed abstract class Parsable[A, C] extends org.opentorah.xml.Parsable[A]:
    def xml(value: A): C

  final class Optional[A](elements: Elements[A]) extends Parsable[Option[A], Option[ScalaXml.Element]]:
    override protected def parser: Parser[Option[A]] = elements.optionalParser
    override def xml(value: Option[A]): Option[ScalaXml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Option[A]] = Unparser[Option[A]](
      content = value => xml(value).toSeq
    )

  final class Required[A](elements: Elements[A]) extends Parsable[A, ScalaXml.Element]:
    override protected def parser: Parser[A] = Effects.required(elements.optionalParser, elements)
    override def xml(value: A): ScalaXml.Element = elements.xmlElement(value)
    override def unparser: Unparser[A] = Unparser[A](
      content = value => Seq(xml(value))
    )

  final class Sequence[A](elements: Elements[A]) extends Parsable[Seq[A], Seq[ScalaXml.Element]]:
    override protected def parser: Parser[Seq[A]] = all(Seq.empty)
    private def all(acc: Seq[A]): Parser[Seq[A]] = for
      next: Option[A] <- elements.optionalParser
      result: Seq[A] <- next
        .map(next => all(acc :+ next))
        .getOrElse(ZIO.succeed(acc))
    yield result
    override def xml(value: Seq[A]): Seq[ScalaXml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Seq[A]] = Unparser[Seq[A]](
      content = value => xml(value)
    )

  // TODO just use XInclude?
  final class Redirect[A](val from: From, elements: Elements[A]):
    def parse: Parser[A] = elements.parse(from)
    def followRedirects: Parser[A] = elements.followRedirects.parse(from)
    def orRedirect: Parser[Redirect.Or[A]] = elements.orRedirect.parse(from)
    def withRedirect(follow: Boolean): Parser[Redirect.Or[A]] = elements.withRedirect(follow).parse(from)

  object Redirect:
    type Or[A] = Either[Redirect[A], A]

  private val redirectAttribute: Attribute[String] = Attribute("file")

  final class HandleRedirect[A, B](
    elements: Elements[A],
    noRedirect: Parser[A] => Parser[B],
    redirected: From => Parser[B]
  ) extends Elements[B]:
    override def elementAndParser(name: String): Option[Element.AndParser[B]] =
      elements.elementAndParser(name).map(_.element).map { element =>
        val parser = for
          url: Option[String] <- redirectAttribute.optional()
          result: B <- url.fold(noRedirect(element.contentParsable().asInstanceOf[Parser[A]]))((url: String) =>
            for
              currentBaseUrl: Option[URL] <- Parsing.currentBaseUrl
              from: URL <- Effects.effect(Files.subUrl(currentBaseUrl, url))
              xml: Xml <- Parsing.currentXml
              result: B <- redirected(From.redirect(from, xml))
            yield result
          )
        yield result
        Element.AndParser[B](element, parser)
      }

    override protected def elementByValue(value: B): Element[?] = ???

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
