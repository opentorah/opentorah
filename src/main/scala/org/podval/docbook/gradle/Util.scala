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

  def subdirectory(directory: File, subdirectoryName: Option[String]): File =
    subdirectoryName.fold(directory)(new File(directory, _))

  def deleteRecursively(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(deleteRecursively)
    if (file.exists && !file.delete)
      throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
  }
}
