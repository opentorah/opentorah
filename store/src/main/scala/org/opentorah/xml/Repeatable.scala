package org.opentorah.xml

import zio.{IO, ZIO}

trait Repeatable[A] extends Optional[A] {

  def optional: Parser[Option[A]]

  def all: Parser[Seq[A]] =
    all(Seq.empty)

  private def all(acc: Seq[A]): Parser[Seq[A]] = for {
    next <- optional
    result <- next.fold[Parser[Seq[A]]](IO.succeed(acc))(next => all(acc :+ next))
  } yield result
}

object Repeatable {

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
