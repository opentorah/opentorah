package org.opentorah.xml

import zio.ZIO

// TODO eliminate casts of results!
// TODO use in calendar...
final class Choice(parsables: Seq[Parsable[_]]) extends Parsable[(Parsable[_], _)] {

  override def contentType: ContentType = {
    require(parsables.nonEmpty)
    val result = parsables.head.contentType
//    require(parsables.forall(_.contentType == result))
//    result
    ContentType.Mixed
  }

  override def name2parser(elementName: String): Option[Parser[(Parsable[_], _)]] = {
    parsables
      .map(parsable => parsable -> parsable.name2parser(elementName)).find(_._2.isDefined)
      .map { case (parsable: Parsable[_], parserOption: Option[Parser[_]]) =>
        val parser: Parser[_] = parserOption.get
        parser.map(result => (parsable, result))
      }
  }

  def toMultiMap: Parser[Map[Parsable[_], Seq[_]]] =
    all.map(result => result.groupBy(_._1).mapValues(_.map(_._2)))

  def toMap: Parser[Map[Parsable[_], _]] =
    toMultiMap.flatMap { result: Map[Parsable[_], Seq[_]] =>
      val tooMany: Seq[Parsable[_]] = result.filter(_._2.length > 1).keys.toSeq
      if (tooMany.nonEmpty) ZIO.fail("Too many values for: " + tooMany.mkString(", "))
      else ZIO.succeed(result.mapValues(_.head))
    }
}

object Choice {

  def apply(parsables: Seq[Parsable[_]]): Choice = new Choice(parsables)
}
