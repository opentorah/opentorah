package org.opentorah.calendar.paper

import java.io.{BufferedWriter, File, FileWriter}

object Table:
  final class Column[T](val heading: String, val f: T => Any)

  def writeAsciidoc(
    directory: File,
    name: String,
    header: Seq[String],
    rows: Seq[Seq[String]]
  ): Unit =
    def formatHeaderRow(values: Seq[String]): String = values.mkString("|", "|", "\n\n")
    def formatRow(values: Seq[String]): String = values.mkString("|", "\n|", "\n\n")

    val strings: Seq[String] =
      Seq(
        "[%autowidth]\n",
        "|===\n",
        formatHeaderRow(header)
      ) ++
        rows.map(formatRow) ++
      Seq(
        "|===\n"
      )

    write(file = File(directory, name + ".adoc"), content = strings.mkString)

  private def write(file: File, content: String): Unit =
    file.getParentFile.mkdirs()
    val writer: BufferedWriter = BufferedWriter(new FileWriter(file))
    try writer.write(content) finally writer.close()

final class Table[T](rows: Seq[T])(columns: Table.Column[T]*):

  def writeAsciidoc(directory: File, name: String): Unit = Table.writeAsciidoc(
    directory,
    name,
    header = columns.map(_.heading),
    rows = rows.map(row => columns.map(_.f(row).toString))
  )
