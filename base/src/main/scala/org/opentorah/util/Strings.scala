package org.opentorah.util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

object Strings:

  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index+1)))

  def splitRight(what: String, on: Char): (Option[String], String) = what.lastIndexOf(on) match
    case -1 => (None, what)
    case index => (Some(what.substring(0, index)), what.substring(index+1))

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

  def spacesToUnderscores(what: String): String = what.replace(' ', '_')
  //def underscoresToSpaces(what: String): String = what.replace('_', ' ')

  def encodeXmlSpecials(string: String): String = string
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")

  def sbToString(f: StringBuilder => Unit): String =
    val sb = new StringBuilder
    f(sb)
    sb.toString

  def empty2none(string: String): Option[String] =
    if string == null || string.isEmpty then None else Some(string)

  def string2stream(string: String): InputStream = ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8))

  def drop(from: String, prefix: String): String =
    if from.startsWith(prefix) then from.substring(prefix.length)
    else throw IllegalArgumentException(s"String '$from' doesn't start with '$prefix'")

  private val hexDigits: Array[Char] = "0123456789abcdef".toCharArray

  def bytes2hex(bytes: Seq[Byte]): String =
    val sb: StringBuilder = StringBuilder(2 * bytes.length)
    for b: Byte <- bytes do sb.append(hexDigits((b >> 4) & 0xf)).append(hexDigits(b & 0xf))
    sb.toString

  def toInt(string: String): Option[Int] =
    try Some(string.toInt) catch
      case _: NumberFormatException => None
