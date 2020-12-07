package org.opentorah.util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.Charset

object Strings {

  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match {
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index+1)))
  }

  def splitRight(what: String, on: Char): (Option[String], String) = what.lastIndexOf(on) match {
    case -1 => (None, what)
    case index => (Some(what.substring(0, index)), what.substring(index+1))
  }

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

  def escape(what: String): String = what
    .replace("\\", "\\\\") // first, so that newly-introduced '\' do not get escaped!
    .replace("\"", "\\\"")
    .replace("\n", "\\n")

  def sbToString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  def empty2none(string: String): Option[String] =
    if (string == null || string.isEmpty) None else Some(string)

  def utf8: Charset = Charset.forName("UTF-8")

  def string2stream(string: String): InputStream = new ByteArrayInputStream(string.getBytes(utf8))

  def drop(from: String, prefix: String): String =
    if (from.startsWith(prefix)) from.substring(prefix.length)
    else throw new IllegalArgumentException(s"String '$from' doesn't start with '$prefix'")
}
