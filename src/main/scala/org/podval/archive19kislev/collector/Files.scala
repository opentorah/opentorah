package org.podval.archive19kislev.collector

import java.io.{BufferedWriter, File, FileWriter}

object Files {
  def listNames(directory: File, extension: String, check: String => Unit): Seq[String] = {
    val result = directory.listFiles.toSeq.map(_.getName).filter(_.endsWith(extension)).map(_.dropRight(extension.length))
    //    result.foreach(check)
    result.sorted
  }

  def write(
    directory: File,
    fileName: String,
    yaml: Seq[(String, String)]
  )(content: Seq[String]): Unit = {
    directory.mkdirs()
    val file = new File(directory, fileName)
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))
    try {
      val result =
        Seq("---") ++
          (for ((name, value) <- yaml) yield name + ": " + value) ++
          Seq("---") ++
          content ++
          Seq("")
      writer.write(result.mkString("\n"))
    } finally {
      writer.close()
    }
  }
}
