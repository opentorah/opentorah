package org.opentorah.xml

import org.opentorah.util.Collections

final class Choice(result: Map[Parsable[_], Seq[_]]) {

  def all[A](parsable: Parsable[A]): Seq[A] =
    result.get(parsable).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)

  def optional[A](parsable: Parsable[A]): Parser[Option[A]] = {
    val result = all(parsable)
    for {
      _ <- Parser.check(result.length <= 1, s"Too many values for $parsable")
    } yield result.headOption
  }
}

object Choice {

  def apply(parsables: Seq[Parsable[_]]): Parser[Choice] = {
    val parsable = new UnionParsable[(Parsable[_], _)](parsables.map { parsable =>
      Parsable.annotate(parsable).asInstanceOf[Parsable[(Parsable[_], _)]]
    })
    val results: Parser[Map[Parsable[_], Seq[_]]] =
      parsable.all.map(parsable =>
        Collections.mapValues(parsable.groupBy(_._1))(_.map(_._2)))

    results.map(new Choice(_))
  }
}
