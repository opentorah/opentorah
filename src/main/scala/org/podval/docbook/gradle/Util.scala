package org.podval.docbook.gradle

import java.io.File

object Util {
  def fileNameWithoutExtension(file: File): String = {
    val result: String = file.getName
    val lastDot: Int = result.lastIndexOf(".")
    result.substring(0, lastDot)
  }

  def drop(what: Seq[String], from: String): Option[String] =
    what.flatMap(drop(_, from)).headOption

  def drop(what: String, from: String): Option[String] =
    if (from.startsWith(what)) Some(from.drop(what.length)) else None
}
