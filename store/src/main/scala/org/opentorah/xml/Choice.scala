package org.opentorah.xml

final class Choice(result: Map[Parsable[_], Seq[_]]) {

  def all[A](parsable: Parsable[A]): Seq[A] =
    result.get(parsable).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty) // TODO yuck!!!

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
      Parsable.annotate(parsable).asInstanceOf[Parsable[(Parsable[_], _)]] // TODO yuck!!!
    })
    val results: Parser[Map[Parsable[_], Seq[_]]] =
      parsable.all.map(_.groupBy(_._1).mapValues(_.map(_._2)))

    results.map(new Choice(_))
  }
}
