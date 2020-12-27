package org.opentorah.collectorng

import org.opentorah.tei.Tei
import org.opentorah.xml.Xml

final class Table[D](columns: Table.Column[D]*) {
  def toTei(rows: Seq[Table.Row[D]]): Xml.Element =
    <table xmlns={Tei.namespace.uri} rendition="collection-index">
      {<row role="label">{columns.map(column => <cell rendition={column.cssClass}>{column.heading}</cell>)}</row>}
      {rows.map {
      case Table.Nodes(nodes) =>
        <row><cell cols={columns.length.toString}><span rendition="part-title">{nodes}</span></cell></row>

      case Table.Data(data) =>
        <row>{columns.map(column => <cell rendition={column.cssClass}>{column.value(data)}</cell>)}</row>
    }}
    </table>
}

object Table {
  final case class Column[D](
    heading: String,
    cssClass: String,
    value: D => Seq[Xml.Node]
  )

  sealed trait Row[+D]

  final case class Data[D](data: D) extends Row[D]

  final case class Nodes(nodes: Seq[Xml.Node]) extends Row[Nothing]
}
