package org.opentorah.xml

import zio.ZIO

trait Parsable[A] {

  def name2parser: Map[String, Parsable.ContentTypeAndParser[A]]

  final def optional: Parser[Option[A]] = for {
    nextName <- Context.nextElementName
    result <- nextName.fold[Parser[Option[A]]](ZIO.none) { nextName =>
      name2parser.get(nextName).fold[Parser[Option[A]]](ZIO.none)(nested(_).map(Some(_)))
    }
  } yield result

  final def required: Parser[A] = for {
    nextName <- Context.nextElementName
    result <- nextName.fold[Parser[A]](ZIO.fail(s"$this required, bot none found")) { nextName =>
      name2parser.get(nextName).fold[Parser[A]](notRecognized(nextName))(nested)
    }
  } yield result

  final def all: Parser[Seq[A]] =
    all(Seq.empty, mustBe = false)

  final def allMustBe: Parser[Seq[A]] =
    all(Seq.empty, mustBe = false)

  private def all(acc: Seq[A], mustBe: Boolean): Parser[Seq[A]] = for {
    nextName <- Context.nextElementName
    result <- nextName.fold[Parser[Seq[A]]](ZIO.succeed(acc)) { nextName =>
      name2parser.get(nextName).fold[Parser[Seq[A]]](if (!mustBe) ZIO.succeed(acc) else notRecognized(nextName)) {
        contentTypeAndParser => for {
          next <- nested(contentTypeAndParser)
          result <- all(acc :+ next, mustBe)
        } yield result
      }
    }
  } yield result

  final def parse(from: From): Parser[A] = for {
    _ <- Context.checkNoLeftovers
    elem <- from.load
    name = elem.label
    result <- name2parser.get(name).fold[Parser[A]](notRecognized(name))(contentTypeAndParser =>
      Context.nested(Some(from), elem, contentTypeAndParser.contentType, contentTypeAndParser.parser))
  } yield result

  private def notRecognized[B](nextName: String): Parser[B] =
    ZIO.fail(s"$this required, but '$nextName' found")

  private final def nested(contentTypeAndParser: Parsable.ContentTypeAndParser[A]): Parser[A] = for {
    nextElement <- Context.nextElement.map(_.get)
    result <- Context.nested(None, nextElement, contentTypeAndParser.contentType, contentTypeAndParser.parser)
  } yield result
}

object Parsable {
  final class ContentTypeAndParser[A](val contentType: ContentType, val parser: Parser[A])

  final def annotate[A](parsable: Parsable[A]): Parsable[(Parsable[A], A)] = new Parsable[(Parsable[A], A)] {
    override def toString: Error = "annotated " + parsable.toString

    override def name2parser: Map[String, ContentTypeAndParser[(Parsable[A], A)]] =
      parsable.name2parser.mapValues[ContentTypeAndParser[(Parsable[A], A)]] { contentTypeAndParser: ContentTypeAndParser[A] =>
        new Parsable.ContentTypeAndParser[(Parsable[A], A)](
          contentTypeAndParser.contentType,
          contentTypeAndParser.parser.map { result: A => parsable -> result }
        )
      }
  }
}
