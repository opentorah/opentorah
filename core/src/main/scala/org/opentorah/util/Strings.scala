package org.opentorah.util

object Strings:

  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index+1)))

  def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  def squashWhitespace(what: String): String = squashBigWhitespace(what)
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
  
  def encodeXmlSpecials(string: String): String = string
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")

  def sbToString(f: scala.collection.mutable.StringBuilder => Unit): String =
    val sb = new scala.collection.mutable.StringBuilder
    f(sb)
    sb.toString
