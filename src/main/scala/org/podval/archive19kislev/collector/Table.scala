package org.podval.archive19kislev.collector

import scala.xml.{Elem, Node}

final class Table[D](columns: Column[D]*) {
  def toTei(rows: Seq[Table.Row[D]]): Elem =
    <table rendition="collection-index">
      <row>{ for (column <- columns) yield <cell rendition={column.cssClass}>{column.heading}</cell> }</row>{
      for (row <- rows) yield row match {
        case Table.Xml(nodes) =>
          <row><cell cols={columns.length.toString}><span rendition="part-title">{nodes}</span></cell></row>

        case Table.Data(data) =>
          <row>{ for (column <- columns) yield <cell rendition={column.cssClass}>{column.value(data)}</cell> }</row>
      }}
    </table>
}

object Table {
  sealed trait Row[+D]

  final case class Data[D](data: D) extends Row[D]

  final case class Xml(nodes: Seq[Node]) extends Row[Nothing]
}
