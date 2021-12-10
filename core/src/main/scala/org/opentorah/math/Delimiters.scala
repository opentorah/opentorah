package org.opentorah.math

import org.opentorah.util.Strings

final class Delimiters(val start: String, val end: String)

object Delimiters:

  // TODO escape backslash here too?
  def toStrings(delimiterss: Seq[Delimiters]): String =
    (for delimiters <- delimiterss.toList yield s"${delimiters.start}-${delimiters.end}").mkString(",")

  def fromStrings(string: String): List[Delimiters] = for string <- Strings.toList(string) yield fromString(string)
    
  def fromString(string: String): Delimiters =
    val parts: Array[String] = string.split("-")
    require(parts.length == 2)
    Delimiters(start = parts(0), end = parts(1))

  def json(delimiterss: Seq[Delimiters]): List[List[String]] = for delimiters <- delimiterss.toList yield List(
    Strings.escape(delimiters.start),
    Strings.escape(delimiters.end)
  )
