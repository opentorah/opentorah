package org.podval.archive19kislev.collector

import java.io.File

object Main {
  val docsDirectory: File = new File("docs").getAbsoluteFile

  val namesFileName: String = "names"

  val collections: Seq[Collection] = Seq(
    new Collection("dubnov", "Дубнов"),
    new Collection("archive", "Архив")
  )

  val names: Names = new Names(docsDirectory, namesFileName)

  def main(args: Array[String]): Unit = {
    println("Writing collection index files.")
    collections.foreach(_.write())

    println("Validating TEI files.")
    val report: Report = Report(collections, names)
    Util.write(Main.docsDirectory, "status.md", report.toStrings)
    if (report.failed)
      throw new IllegalArgumentException("\nTEI valisation failed!\n" + report.toStrings.mkString("\n"))
  }
}
