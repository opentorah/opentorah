package org.opentorah.xml

import org.opentorah.util.{Collections, Effects, Files}
import zio.ZIO
import java.net.URL

trait Elements[A] {

  protected def elementByName(name: String): Option[Element[_]]

  protected def elementByValue(value: A): Element[_]

  protected def mapParser(element: Element[_], parser: Parser[_]): Parser[A]

  def xmlElement(value: A): ScalaXml.Element =
    elementByValue(value).asInstanceOf[Element[A]].xmlElement(value)

  private def nested(
    from: Option[From],
    nextElement: ScalaXml.Element,
    element: Element[_]
  ): Parser[A] = Context.nested(
    from,
    nextElement,
    element.contentType,
    mapParser(element, element.contentParsable())
  )

  private def optionalParser: Parser[Option[A]] = for {
    // TODO take namespace into account!
    nextElement <- Context.nextElement(element => elementByName(ScalaXml.getName(element)).isDefined)
    result <- nextElement
      .map(nextElement => nested(
        from = None,
        nextElement,
        element = elementByName(ScalaXml.getName(nextElement)).get
      ).map(Some(_)))
      .getOrElse(ZIO.none)
  } yield result

  final def parse(from: From): Parser[A] = for {
    _ <- Context.checkNoLeftovers
    nextElement <- from.load
    elementName = ScalaXml.getName(nextElement)
    result <- elementByName(elementName)
      .map(element => nested(
        Some(from),
        nextElement,
        element
      ))
      .getOrElse(ZIO.fail(s"$this required, but '$elementName' found"))
  } yield result

  final def parse(fromUrl: URL): Parser[A] = parse(From.url(fromUrl))

  final def optional: Elements.Optional[A] = new Elements.Optional[A](this)
  final def required: Elements.Required[A] = new Elements.Required[A](this)
  final def seq     : Elements.Sequence[A] = new Elements.Sequence[A](this)

  final def followRedirects: Elements.HandleRedirect[A, A] = followRedirects(identity)

  private def followRedirects[B](f: Parser[A] => Parser[B]): Elements.HandleRedirect[A, B] = new Elements.HandleRedirect(
    this,
    noRedirect = f,
    redirected = url => followRedirects(f).parse(From.redirect(url))
  )

  final def orRedirect: Elements.HandleRedirect[A, Redirect.Or[A]] = new Elements.HandleRedirect(
    this,
    noRedirect = _.map(Right(_)),
    redirected = url => ZIO.left(new Redirect[A](url, this))
  )

  final def withRedirect(follow: Boolean): Elements.HandleRedirect[A, Redirect.Or[A]] =
    if (!follow) orRedirect else followRedirects(_.map(Right(_)))
}

object Elements {

  sealed abstract class Parsable[A, C] extends org.opentorah.xml.Parsable[A] {
    def xml(value: A): C
  }

  final class Optional[A](elements: Elements[A]) extends Parsable[Option[A], Option[ScalaXml.Element]] {
    override protected def parser: Parser[Option[A]] = elements.optionalParser
    override def xml(value: Option[A]): Option[ScalaXml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Option[A]] = Unparser[Option[A]](
      content = value => xml(value).toSeq
    )
  }

  final class Required[A](elements: Elements[A]) extends Parsable[A, ScalaXml.Element] {
    override protected def parser: Parser[A] = Effects.required(elements.optionalParser, elements)
    override def xml(value: A): ScalaXml.Element = elements.xmlElement(value)
    override def unparser: Unparser[A] = Unparser[A](
      content = value => Seq(xml(value))
    )
  }

  final class Sequence[A](elements: Elements[A]) extends Parsable[Seq[A], Seq[ScalaXml.Element]]{
    override protected def parser: Parser[Seq[A]] = all(Seq.empty)
    private def all(acc: Seq[A]): Parser[Seq[A]] = for {
      next <- elements.optionalParser
      result <- next
        .map(next => all(acc :+ next))
        .getOrElse(ZIO.succeed(acc))
    } yield result
    override def xml(value: Seq[A]): Seq[ScalaXml.Element] = value.map(elements.xmlElement)
    override def unparser: Unparser[Seq[A]] = Unparser[Seq[A]](
      content = value => xml(value)
    )
  }

  private val redirectAttribute: Attribute[String] = Attribute("file")

  final class HandleRedirect[A, B](
    elements: Elements[A],
    noRedirect: Parser[A] => Parser[B],
    redirected: URL => Parser[B]
  ) extends Elements[B] {
    override protected def elementByName(name: String): Option[Element[_]] =
      elements.elementByName(name)

    override protected def mapParser(element: Element[_], parser: Parser[_]): Parser[B] = for {
      url <- Elements.redirectAttribute.optional()
      result <- url.fold(noRedirect(parser.asInstanceOf[Parser[A]])) { (url: String) =>
        for {
          currentBaseUrl <- Context.currentBaseUrl
          from <- Effects.effect(Files.subUrl(currentBaseUrl, url))
          result <- redirected(from)
        } yield result
      }
    } yield result

    override protected def elementByValue(value: B): Element[_] = ???
  }

  abstract class Union[A] extends Elements[A] {
    protected def elements: Seq[Element[_ <: A]]

    final override protected def elementByName(name: String): Option[Element[_ <: A]] =
      elements.find(_.elementName == name)

    final override protected def mapParser(element: Element[_], parser: Parser[_]): Parser[A] =
      parser.asInstanceOf[Parser[A]]
  }

  final class Choices(result: Map[Element[_], Seq[_]]) {

    def all[A](element: Element[A]): Seq[A] =
      result.get(element).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)

    def optional[A](element: Element[A]): Parser[Option[A]] = {
      val results = all(element)
      for {
        _ <- Effects.check(results.length <= 1, s"Too many values for $element")
      } yield results.headOption
    }
  }

  private final class Choice(elements: Seq[Element[_]]) extends Elements[(Element[_], _)] {
    override protected def elementByName(name: String): Option[Element[_]] =
      elements.find(_.elementName == name)

    override protected def mapParser(element: Element[_], parser: Parser[_]): Parser[(Element[_], _)] =
      parser.map(result => element -> result)

    override protected def elementByValue(value: (Element[_], _)): Element[_] =
      throw new UnsupportedOperationException("Not supposed to happen!")
  }

  def choices(elements: Seq[Element[_]]): Parser[Choices] = new Choice(elements)
    .seq()
    .map((results: Seq[(Element[_], _)]) => Collections.mapValues(results.groupBy(_._1))(_.map(_._2)))
    .map(new Choices(_))
}
