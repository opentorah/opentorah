package org.opentorah.xml

import org.opentorah.util.Collections

final class Choice(result: Map[Element[_], Seq[_]]) {

  private def all[A](element: Element[A]): Seq[A] =
    result.get(element).map(_.map(_.asInstanceOf[A])).getOrElse(Seq.empty)

  def optional[A](element: Element[A]): Parser[Option[A]] = {
    val results = all(element)
    for {
      _ <- Parser.check(results.length <= 1, s"Too many values for $element")
    } yield results.headOption
  }
}

// TODO clean up
// - remove new FromXml()
// - make canParse() less accessible
// - move into FromXml
object Choice {

  def apply(elements: Seq[Element[_]]): Parser[Choice] = new FromXml[(Element[_], _)] {
    override def canParse(elementName: String): Option[CanParse[(Element[_], _)]] = elements
      .find(_.elementName == elementName)
      .map { element => element
        .mustParse(elementName)
        .mapParser(parser => parser.map(result => element -> result))
      }
  }
    .all
    .map((results: Seq[(Element[_], _)]) => Collections.mapValues(results.groupBy(_._1))(_.map(_._2)))
    .map(new Choice(_))
}
