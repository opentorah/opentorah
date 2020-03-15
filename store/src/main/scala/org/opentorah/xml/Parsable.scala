package org.opentorah.xml

import zio.ZIO
import scala.xml.Elem

trait Parsable[A] {

  def name2parser(elementName: String): Option[Parser[A]]

  def contentType: ContentType

  final val nextName: Parser[Option[String]] =
    Context.lift(current => Content.getNextElementName(current.content))

  final val name: Parser[String] =
    Context.lift(_.name)

  final def optional: Parser[Option[A]] = for {
    nextName <- nextName
    result <- nextName.fold[Parser[Option[A]]](ZIO.none) { nextName =>
      name2parser(nextName).fold[Parser[Option[A]]](ZIO.none)(nextParser => nested(nextParser).map(Some(_)))
    }
  } yield result

  final def required: Parser[A] = for {
    nextName <- nextName
    result <- nextName.fold[Parser[A]](ZIO.fail(s"$this required, bot none found")) { nextName =>
      name2parser(nextName).fold[Parser[A]](notRecognized(nextName))(nextParser => nested(nextParser))
    }
  } yield result

  final def topLevel: Parser[A] = for {
    name <- name
    result <- name2parser(name).fold[Parser[A]](notRecognized(name))(identity)
  } yield result

  final def all: Parser[Seq[A]] =
    all(Seq.empty, mustBe = false)

  final def allMustBe: Parser[Seq[A]] =
    all(Seq.empty, mustBe = false)

  private def all(acc: Seq[A], mustBe: Boolean): Parser[Seq[A]] = for {
    nextName <- nextName
    result <- nextName.fold[Parser[Seq[A]]](ZIO.succeed(acc)) { nextName =>
      name2parser(nextName).fold[Parser[Seq[A]]](if (!mustBe) ZIO.succeed(acc) else notRecognized(nextName)) { nextParser =>
        for {
          next <- nested(nextParser)
          result <- all(acc :+ next, mustBe)
        } yield result
      }
    }
  } yield result

  private def notRecognized[B](nextName: String): Parser[B] =
    ZIO.fail(s"$this required, but $nextName found")

  private final def nested(parser: Parser[A]): Parser[A] = for {
    nextElement <- nextElement.map(_.get)
    result <- Context.nested(None, nextElement, contentType, parser)
  } yield result

  private final val nextElement: Parser[Option[Elem]] =
    Context.liftContentModifier(Content.takeNextElement)

  def toXml(value: A): Elem

  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)
}
