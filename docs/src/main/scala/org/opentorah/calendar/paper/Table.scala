package org.opentorah.calendar.paper

import org.opentorah.util.Files
import org.opentorah.xml.ScalaXml
import java.io.File

object Table:
  final class Column[T](val heading: String, val f: T => Any)

final class Table[T](rows: Seq[T])(columns: Table.Column[T]*):

  def writeAsciidoc(directory: File, name: String): Unit =
    def formatRow(values: Seq[String]): String = values.mkString("|", "\n|", "\n\n")

    val strings: Seq[String] = Seq(
      s"""[%header, cols="${"1".repeat(columns.length).mkString(", ")}"]\n""",
      "|===\n",
      formatRow(columns.map(_.heading))
    ) ++
      rows.map(row => formatRow(columns.map(_.f(row).toString))) ++ Seq(
      "|===\n"
    )

    Files.write(file = File(directory, name + ".adoc"), content = strings.mkString)

//  def writeMarkdown(directory: File, name: String): Unit =
//    def formatRow(values: Seq[String]): String = values.mkString("|", "|", "|\n")
//
//    val strings: Seq[String] = Seq(
//      formatRow(columns.map(_.heading)),
//      formatRow(columns.map(_ => "---:"))
//    ) ++
//      rows.map(row => formatRow(columns.map(_.f(row).toString)))
//
//    Files.write(file = File(directory, name + ".md"), content = strings.mkString)
