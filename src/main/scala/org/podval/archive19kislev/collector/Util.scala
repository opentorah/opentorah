package org.podval.archive19kislev.collector

import java.io.{BufferedWriter, File, FileWriter}

object Util {
  def write(
    directory: File,
    fileName: String,
    content: Seq[String]
  ): Unit = write(directory, fileName, content.mkString("\n"))

  def write(
    directory: File,
    fileName: String,
    content: String
  ): Unit = {
    directory.mkdirs()
    val file = new File(directory, fileName)
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }

  def removeConsecutiveDuplicates[T](seq: Seq[T]): Seq[T] = seq match {
    case Nil => Nil
    case x :: y :: xs if x == y => removeConsecutiveDuplicates(y :: xs)
    case x :: xs => x +: removeConsecutiveDuplicates(xs)
  }
}
