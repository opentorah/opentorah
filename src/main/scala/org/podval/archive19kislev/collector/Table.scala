package org.podval.archive19kislev.collector

import scala.xml.{Elem, Node}

final class Table[D](preRow: D => Seq[Node], columns: Column[D]*) {
  def toTei(data: Seq[D]): Elem =
    <table rendition="collection-index">
      <row>{ for (column <- columns) yield <cell rendition={column.cssClass}>{column.heading}</cell> }</row>
      { data.flatMap { rowData =>
      preRow(rowData).flatMap(line => <row><cell cols={columns.length.toString}>{line}</cell></row>) ++
      <row>{for (column <- columns) yield <cell rendition={column.cssClass}>{column.value(rowData)}</cell>}</row>
      }}
    </table>
}
