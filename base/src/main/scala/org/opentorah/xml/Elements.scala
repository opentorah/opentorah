package org.opentorah.xml

import org.opentorah.util.{Collections, Effects, Files}
import zio.ZIO
import java.net.URL

trait Elements[A]:

  protected def elementByName(name: String): Option[Element[?]]

  protected def elementByValue(value: A): Element[?]

  protected def mapParser(element: Element[?], parser: Parser[?]): Parser[A]

  def xmlElement(value: A): ScalaXml.Element =
    elementByValue(value).asInstanceOf[Element[A]].xmlElement(value)

  private def nested(
    from: Option[From],
    nextElement: ScalaXml.Element,
    element: Element[?]
  ): Parser[A] = Context.nested(
    from,
    nextElement,
    element.contentType,
    mapParser(element, element.contentParsable())
  )

  private def optionalParser: Parser[Option[A]] = for
    // TODO take namespace into account!
    nextElementOpt: Option[ScalaXml.Element] <- Context.nextElement(element => elementByName(ScalaXml.getName(element)).isDefined)
    result: Option[A] <- if nextElementOpt.isEmpty then ZIO.none else
      val nextElement: ScalaXml.Element = nextElementOpt.get
      val nextElementName:String = ScalaXml.getName(nextElement)
      val nextElementParser: Element[?] = elementByName(nextElementName).get
      for
        result: A <- nested(
          from = None,
          nextElement,
          element = nextElementParser
        )
      yield Some(result)
  yield result

  final def parse(from: From): Parser[A] = for
    _ <- Context.checkNoLeftovers
    nextElement: ScalaXml.Element <- from.load
    nextElementName: String = ScalaXml.getName(nextElement)
    nextElementParserOpt: Option[Element[_]] = elementByName(nextElementName)
    result: A <- if nextElementParserOpt.isEmpty then Effects.fail(s"$this required, but '$nextElementName' found") else
      nested(
        Some(from),
        nextElement,
        nextElementParserOpt.get
      )
  yield result

  final def parse(fromUrl: URL): Parser[A] = parse(From.url(fromUrl))

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
    redirected = url => followRedirects(f).parse(From.redirect(url))
  )

  final def orRedirect: Elements.HandleRedirect[A, Redirect.Or[A]] = Elements.HandleRedirect(
    this,
    noRedirect = _.map(Right(_)),
    redirected = url => ZIO.left(Redirect[A](url, this))
  )

  final def withRedirect(follow: Boolean): Elements.HandleRedirect[A, Redirect.Or[A]] =
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

  private val redirectAttribute: Attribute[String] = Attribute("file")

  final class HandleRedirect[A, B](
    elements: Elements[A],
    noRedirect: Parser[A] => Parser[B],
    redirected: URL => Parser[B]
  ) extends Elements[B]:
    override protected def elementByName(name: String): Option[Element[?]] =
      elements.elementByName(name)

    override protected def mapParser(element: Element[?], parser: Parser[?]): Parser[B] = for
      url: Option[String] <- Elements.redirectAttribute.optional()
      result: B <- url.fold(noRedirect(parser.asInstanceOf[Parser[A]]))((url: String) =>
        for
          currentBaseUrl: Option[URL] <- Context.currentBaseUrl
          from: URL <- Effects.effect(Files.subUrl(currentBaseUrl, url))
          result: B <- redirected(from)
        yield result
      )
    yield result

    override protected def elementByValue(value: B): Element[?] = ???

  abstract class Union[A] extends Elements[A]:
    protected def elements: Seq[Element[? <: A]]

    final override protected def elementByName(name: String): Option[Element[? <: A]] =
      elements.find(_.elementName == name)

    final override protected def mapParser(element: Element[?], parser: Parser[?]): Parser[A] =
      parser.asInstanceOf[Parser[A]]

  final class Choices(result: Map[Element[?], Seq[?]]):

    def all[A](element: Element[A]): Seq[A] =
      result.get(element).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)

    def optional[A](element: Element[A]): Parser[Option[A]] =
      val results = all(element)
      for
        _ <- Effects.check(results.length <= 1, s"Too many values for $element")
      yield results.headOption

  private final class Choice(elements: Seq[Element[?]]) extends Elements[(Element[?], ?)]:
    override protected def elementByName(name: String): Option[Element[?]] =
      elements.find(_.elementName == name)

    override protected def mapParser(element: Element[?], parser: Parser[?]): Parser[(Element[?], ?)] =
      parser.map(result => element -> result)

    override protected def elementByValue(value: (Element[?], ?)): Element[?] =
      throw UnsupportedOperationException("Not supposed to happen!")

  def choices(elements: Seq[Element[?]]): Parser[Choices] = Choice(elements)
    .seq()
    .map((results: Seq[(Element[?], ?)]) => Collections.mapValues(results.groupBy(_._1))(_.map(_._2)))
    .map(Choices(_))
