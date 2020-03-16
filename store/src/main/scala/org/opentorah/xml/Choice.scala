package org.opentorah.xml

import zio.ZIO

// TODO eliminate casts of results!
// TODO use in calendar...
final class Choice(toMultiMap: Parser[Map[Parsable[_], Seq[_]]]) {
  def toMap: Parser[Map[Parsable[_], _]] =
    toMultiMap.flatMap { result: Map[Parsable[_], Seq[_]] =>
      val tooMany: Seq[Parsable[_]] = result.filter(_._2.length > 1).keys.toSeq
      if (tooMany.nonEmpty) ZIO.fail("Too many values for: " + tooMany.mkString(", "))
      else ZIO.succeed(result.mapValues(_.head))
    }
}

object Choice {

  def apply(parsables: Seq[Parsable[_]]): Choice = {
    val parsable = new UnionParsable[(Parsable[_], _)](parsables.map { parsable: Parsable[_] =>
      Parsable.annotate(parsable).asInstanceOf[Parsable[(Parsable[_], _)]] // TODO yuck!!!
    })
    val results: Parser[Map[Parsable[_], Seq[_]]] = parsable.all.map(result => result.groupBy(_._1).mapValues(_.map(_._2)))
    new Choice(results)
  }
}
