package org.podval.archive19kislev.collector

import scala.xml.{Elem, Node, Text, TopScope}

final class Column[D](
  val heading: String,
  val cssClass: String,
  val value: D => Seq[Node]
)

object Column {

  def elem[D](heading: String, cssClass: String, value: D => Option[Elem]): Column[D] = new Column[D](
    heading,
    cssClass,
    (data: D) => value(data).fold[Seq[Node]](Text(""))(_.child.map(removeNamespace))
  )

  def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def string[D](heading: String, cssClass: String, value: D => Option[String]) : Column[D] = new Column[D](
    heading,
    cssClass,
    (data: D) => value(data).fold[Seq[Node]](Text(""))(value => Text(value))
  )
}
