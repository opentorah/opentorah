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

// TODO clean up
object Choice {

  def apply(parsables: Seq[Element.WithToXml[_]]): Parser[Choice] = {
    val parsable: Parsable[(Parsable[_], _)] = new Parsable[(Parsable[_], _)] {
      override def canParse(elementName: String): Option[ToParse[(Parsable[_], _)]] =
        parsables.find(_.elementName == elementName).map(parsable => annotate(parsable).mustParse(elementName))
    }

    val results: Parser[Map[Parsable[_], Seq[_]]] =
      parsable.all.map(parsable =>
        Collections.mapValues(parsable.groupBy(_._1))(_.map(_._2)))

    results.map(new Choice(_))
  }

  final def annotate[A](parsable: Parsable[A]): Parsable[(Parsable[A], A)] = new Parsable[(Parsable[A], A)] {
    override def toString: Error = "annotated " + parsable.toString

    override def canParse(elementName: String): Option[ToParse[(Parsable[A], A)]] =
      parsable.canParse(elementName).map { toParse: ToParse[A] =>
        new ToParse[(Parsable[A], A)] {
          override def contentType: ContentType = toParse.contentType
          override def parser: Parser[(Parsable[A], A)] = toParse.parser.map { result => parsable -> result }
        }
      }
  }
}
