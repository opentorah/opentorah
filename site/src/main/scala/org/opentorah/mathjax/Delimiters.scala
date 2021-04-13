package org.opentorah.mathjax

import org.opentorah.util.Strings

final class Delimiters(val start: String, val end: String)

object Delimiters {
  def apply(delimiter: String): Seq[Delimiters] = Seq(new Delimiters(start = delimiter, end = delimiter))

  def json(delimiterss: Seq[Delimiters]): List[Any] = delimiterss.toList.map(delimiters => List(
    Strings.escape(delimiters.start),
    Strings.escape(delimiters.end)
  ))
}
