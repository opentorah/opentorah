package org.opentorah.xml

import org.opentorah.util.Files
import zio.ZIO
import java.net.URL

abstract class Element[A](val elementName: String) extends FromXml[A] with ToXml[A] {

  def contentType: ContentType = ContentType.Elements

  def parser: Parser[A]

  override def toString: String = s"element '$elementName' [$contentType]"

  private[xml] final override def canParse(elementName: String): Option[CanParse[A]] =
    if (elementName != this.elementName) None else Some(new CanParse[A](contentType, parser))

  private def handleRedirect[B](
    noRedirect: Parser[A] => Parser[B],
    redirected: URL => Parser[B]
  ): FromXml[B] = mapParser(parser => for {
    url <- Element.redirectAttribute.optional
    result <- url.fold(noRedirect(parser)) { url: String =>
      for {
        currentBaseUrl <- Context.currentBaseUrl
        from <- Parser.effect(Files.subUrl(currentBaseUrl, url))
        result <- redirected(from)
      } yield result
    }
  } yield result)

  final def followRedirects: FromXml[A] = handleRedirect(
    noRedirect = identity,
    redirected = url => followRedirects.parse(From.redirect(url))
  )

  final def orRedirect: FromXml[Redirect.Or[A]] = handleRedirect(
    noRedirect = _.map(Right(_)),
    redirected = url => ZIO.left(new Redirect[A](url, this))
  )

  final def withRedirect(follow: Boolean): FromXml[Redirect.Or[A]] =
    if (!follow) orRedirect else followRedirects.mapParser(_.map(Right(_)))

  override def toXmlElement(value: A): Xml.Element = Xml.construct(
    name = elementName,
    namespace = antiparser.namespace,
    attributes = antiparser.attributes(value),
    children = antiparser.content(value)
  )

  // TODO make protected again?
  def antiparser: Antiparser[A]
}

object Element {
  private val redirectAttribute: Attribute[String] = Attribute("file")

  abstract class WithToXmlFromUrl[A <: FromUrl.With](elementName: String) extends Element[A](elementName) {
    final def withRedirect(fromUrl: FromUrl.With, follow: Boolean): ToXml[A] = new ToXml[A] {
      override def toXmlElement(value: A): Xml.Element =
        if (follow || value.fromUrl.inline) WithToXmlFromUrl.this.toXmlElement(value)
        else constructRedirect(relativize(value.fromUrl.url, fromUrl.fromUrl.url).toString)
    }

    private def relativize(url: URL, base: URL): URL =
      url.toURI.relativize(base.toURI).toURL

    private def constructRedirect(redirect: String): Xml.Element = Xml.construct(
      name = elementName,
      namespace = None,
      attributes = Seq(redirectAttribute.withValue(redirect)),
      children = Seq.empty
    )
  }

  abstract class Union[A] extends FromXml[A] /* TODO with ToXml[A]*/ {
    final override def canParse(elementName: String): Option[CanParse[A]] =
      elements.find(_.elementName == elementName).map(_.mustParse(elementName))

    protected def elements: Seq[Element[_ <: A]]

//    final override def toXmlElement(value: A): Elem =
//      forValue(value).asInstanceOf[Element[A]].toXmlElement(value)
//
//    protected def forValue(value: A): Element[_ <: A]
  }

  val allNodes: Parser[Seq[Xml.Node]] =
    Context.allNodes

  val allAttributes: Parser[Seq[Attribute.Value[String]]] =
    Context.takeAllAttributes
}
