package org.opentorah.importers.tanach.jerusalem

final class Line(var line: String) {
  def isEmpty: Boolean = line.isEmpty

  def size: Int = line.size

  def indexOf(what: String): Int = line.indexOf(what)

  def consumeToSpace(): String = consumeToIndex(line.indexOf(" "))

  def consumeBracketed(): Option[String] = {
    if (line.startsWith("[")) {
      val index = line.indexOf("]")
      val bracketed = consumeToIndex(index+1)
      val result = bracketed.drop(1).dropRight(1)
      Some(result)
    } else {
      None
    }
  }

  def consume(what: String): Boolean = {
    val result = line.startsWith(what)
    if (result) {
      consumeToIndex(what.length())
    }
    result
  }

  def consumeToIndex(index: Int): String = {
    val result = line.take(index)
    line = line.drop(index).trim()
    result
  }
}
