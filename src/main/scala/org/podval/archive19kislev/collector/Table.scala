package org.podval.archive19kislev.collector

import scala.xml.{Elem, Node}

final class Table[D](preRow: D => Seq[Node], columnsRaw: (String, D => Seq[Node])*) {
  private val columns: Seq[Table.Column[D]] =
    for ((heading, value) <- columnsRaw) yield new Table.Column[D](heading, value)

  def toTei(data: Seq[D]): Elem =
    <table rendition="collection-index">
      <row>{ for (column <- columns) yield <cell>{column.heading}</cell> }</row>
      { data.flatMap { rowData =>
      preRow(rowData).flatMap(line => <row><cell cols={columns.length.toString}>{line}</cell></row>) ++
      <row>{for (column <- columns) yield <cell>{column.value(rowData)}</cell>}</row>
      }}
    </table>
}

object Table {
  final class Column[D](
    val heading: String,
    val value: D => Seq[Node]
  )
}
