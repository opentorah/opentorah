package org.opentorah.xml

import zio.{IO, ZIO}

trait Parsable[A] {

  def optional: Parser[Option[A]]

  def required: Parser[A] =
    Parser.required(this.toString, optional)

  def all: Parser[Seq[A]] =
    Parser.all(optional)
}

object Parsable {

  // TODO eliminate casts of results!

  // TODO use in calendar...
  def optional(parsables: Seq[Parsable[_]]): Parser[Option[(Parsable[_], _)]] =
    if (parsables.isEmpty) ZIO.none else for {
      next <- parsables.head.optional
      result <- if (next.isEmpty) optional(parsables.tail) else ZIO.some(parsables.head -> next.get)
    } yield result

  def all(parsables: Seq[Parsable[_]]): Parser[Map[Parsable[_], Seq[_]]] =
    Parser.all(optional(parsables))
      .map(result => result.groupBy(_._1).mapValues(_.map(_._2)))

  def allAtMostOnce(parsables: Seq[Parsable[_]]): Parser[Map[Parsable[_], _]] =
    all(parsables).flatMap { result: Map[Parsable[_], Seq[_]] =>
      val tooMany: Seq[Parsable[_]] = result.filter(_._2.length > 1).keys.toSeq
      if (tooMany.nonEmpty) IO.fail("Too many values for: " + tooMany.mkString(", "))
      else IO.succeed(result.mapValues(_.head))
    }
}
