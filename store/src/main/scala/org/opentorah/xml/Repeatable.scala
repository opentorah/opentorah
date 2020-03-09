package org.opentorah.xml

import zio.{IO, ZIO}
import scala.xml.Elem

trait Repeatable[A] extends Optional[A] {

  def optional: Parser[Option[A]]

  final def all: Parser[Seq[A]] =
    all(Seq.empty)

  private def all(acc: Seq[A]): Parser[Seq[A]] = for {
    next <- optional
    result <- next.fold[Parser[Seq[A]]](IO.succeed(acc))(next => all(acc :+ next))
  } yield result
}

object Repeatable {

  abstract class WithElementName[A](elementName: String) extends Repeatable[A] {

    final override def optional: Parser[Option[A]] = for {
      hasNext <- Element.nextNameIs(elementName)
      result <- if (!hasNext) ZIO.none else for {
        nextElement <- Element.nextElement.map(_.get)
        result <- parse(nextElement)
      } yield Some(result)
    } yield result

    protected def parse(elem: Elem): Parser[A]
  }

  // TODO eliminate casts of results!
  // TODO use in calendar...
  final class Choice(repeatables: Seq[Repeatable[_]]) extends Repeatable[(Repeatable[_], _)] {
    override def optional: Parser[Option[(Repeatable[_], _)]] =
      optional(repeatables)

    private def optional(repeatables: Seq[Repeatable[_]]): Parser[Option[(Repeatable[_], _)]] =
      if (repeatables.isEmpty) ZIO.none else for {
        next <- repeatables.head.optional
        result <- if (next.isEmpty) optional(repeatables.tail) else ZIO.some(repeatables.head -> next.get)
      } yield result

    def toMultiMap: Parser[Map[Repeatable[_], Seq[_]]] =
      all.map(result => result.groupBy(_._1).mapValues(_.map(_._2)))

    def toMap: Parser[Map[Repeatable[_], _]] =
      toMultiMap.flatMap { result: Map[Repeatable[_], Seq[_]] =>
        val tooMany: Seq[Repeatable[_]] = result.filter(_._2.length > 1).keys.toSeq
        if (tooMany.nonEmpty) IO.fail("Too many values for: " + tooMany.mkString(", "))
        else IO.succeed(result.mapValues(_.head))
      }
  }

  def choice(repeatables: Seq[Repeatable[_]]): Choice = new Choice(repeatables)
}
