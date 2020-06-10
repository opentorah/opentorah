package org.opentorah.xml

import java.net.URL
import org.opentorah.util.Collections
import zio.ZIO
import scala.xml.{Elem, Node}

trait Parsable[A] {

  def name2parser: Map[String, Parsable.ContentTypeAndParser[A]]

  final val optional: Parser[Option[A]] = for {
    elementOpt <- Context.nextElement(element => name2parser.keySet.contains(element.label))
    result <- elementOpt.fold[Parser[Option[A]]](ZIO.none)(element =>
      nested(None, element, name2parser(element.label)).map(Some(_)))
  } yield result

  final val required: Parser[A] = for {
    opt <- optional
    result <- opt.fold[Parser[A]](ZIO.fail(s"$this required, bot none found"))(ZIO.succeed[A](_))
  } yield result

  final def all: Parser[Seq[A]] = all(Seq.empty)

  private def all(acc: Seq[A]): Parser[Seq[A]] = for {
    opt <- optional
    result <- opt.fold[Parser[Seq[A]]](ZIO.succeed(acc))(next => all(acc :+ next))
  } yield result

  final def parse(fromUrl: URL): Parser[A] = parse(From.url(fromUrl))

  final def parse(from: From): Parser[A] = for {
    _ <- Context.checkNoLeftovers
    nextElement <- from.load
    name = nextElement.label
    result <- name2parser.get(name)
      .fold[Parser[A]](ZIO.fail(s"$this required, but '$name' found"))(contentTypeAndParser =>
        nested(Some(from), nextElement, contentTypeAndParser))
  } yield result

  private def nested(
    from: Option[From],
    nextElement: Elem,
    contextTypeAndParser: Parsable.ContentTypeAndParser[A]
  ): Parser[A] = for {
    newCurrent <- Current.open(from, nextElement, contextTypeAndParser.contentType)
    result <- Context.nested(newCurrent, contextTypeAndParser.parser)
  } yield result

  final def descendants(xml: Node): Seq[A] =
    for (xml <- name2parser.keys.toSeq.flatMap(Xml.descendants(xml, _)))
    yield Parser.parseDo(parse(From.xml("descendants", xml)))
}

object Parsable {
  final class ContentTypeAndParser[A](val contentType: ContentType, val parser: Parser[A])

  final def annotate[A](parsable: Parsable[A]): Parsable[(Parsable[A], A)] = new Parsable[(Parsable[A], A)] {
    override def toString: Error = "annotated " + parsable.toString

    override val name2parser: Map[String, ContentTypeAndParser[(Parsable[A], A)]] =
      Collections.mapValues(parsable.name2parser) { contentTypeAndParser: ContentTypeAndParser[A] =>
        new Parsable.ContentTypeAndParser[(Parsable[A], A)](
          contentTypeAndParser.contentType,
          contentTypeAndParser.parser.map { result => parsable -> result }
        )
      }
  }

  def withInclude[A](parsable: Parsable[A], attributeName: String = "include"): Parsable[A] = new Parsable[A] {
    override def toString: String = parsable.toString + s" with include [$attributeName]"

    override val name2parser: Map[String, ContentTypeAndParser[A]] =
      (for {
        (elementName, contentTypeAndParser) <- parsable.name2parser
        contentType = contentTypeAndParser.contentType
        parser = contentTypeAndParser.parser
      } yield (elementName, new ContentTypeAndParser[A](contentType,
        for {
          url <- Attribute(attributeName).optional
          result <- url.fold(parser) { url => for {
            currentFromUrl <- Context.currentFromUrl
            from <- Parser.effect(From.url(currentFromUrl.fold(new URL(url))(new URL(_, url))))
            result <- new Element[A](elementName, contentType, parser).parse(from)
          } yield result}
        } yield result
      ))).toMap
  }
}
