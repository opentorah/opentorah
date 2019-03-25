package org.podval.archive19kislev.collector

final class Table[D](preRow: D => Seq[String], columnsRaw: (String, D => AnyRef)*) {
  private val columns: Seq[Table.Column[D]] =
    for ((heading, value) <- columnsRaw) yield new Table.Column[D](heading, value)

  def toMarkdown(data: Seq[D]): Seq[String] = header ++ data.flatMap { rowData =>
    preRow(rowData).map(line => s"| $line |") ++
    row(rowData)
  }

  private def header: Seq[String] = Seq(
    row(_.heading),
    row(_ => "---")
  )

  private def row(data: D): Seq[String] =
    Seq[String](row(column => toMarkdown(column.value(data))))

  private def row(cell: Table.Column[D] => String): String =
    columns.map(cell).mkString("| ", " | ", " |")

  private def toMarkdown(value: AnyRef): String = value match {
    case link: Link => link.toMarkdown
    case links: Seq[Link] => links.map(_.toMarkdown).mkString(" ")
    case other => other.toString
  }
}

object Table {
  final class Column[D](
    val heading: String,
    val value: D => AnyRef
  )
}
