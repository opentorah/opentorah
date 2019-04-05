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
    collections.foreach(_.write())
    statusReport(Report(collections, names))
  }

  private def statusReport(report: Report): Unit =
    Util.write(docsDirectory, "status.md", report.toStrings)
}
