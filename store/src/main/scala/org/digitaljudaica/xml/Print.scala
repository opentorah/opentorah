package org.digitaljudaica.xml

import scala.xml.Elem

object Print {

  private val width: Int = 120

  private val indent: Int = 2

  private val prettyPrinter: scala.xml.PrettyPrinter = new scala.xml.PrettyPrinter(width, indent)

  private val join: Set[String] = Set(".", ",", ";", ":", "\"", ")")

  def format(elem: Elem): String = {
    val result: String = prettyPrinter.format(elem)

    // pretty-printer splits punctuation from the preceding elements; merge them back :)
    merge(List.empty, result.split("\n").toList).mkString("\n")
  }

  def render(elem: Elem): String = {
    def serialize(string: String): String =
      //scala.xml.Utility.serialize(Parser.run(From.string(string).load)).toString
      Parser.run(From.string(string).load).toString

    val result: String = new PaigesPrettyPrinter(width, indent).render(elem)
    val oldSerialized: String = serialize(format(elem))
    val newSerialized: String = serialize(result)
    if (oldSerialized != newSerialized) {
      val x = 0
    }
    result
  }

  @scala.annotation.tailrec
  private def merge(result: List[String], lines: List[String]): List[String] = lines match {
    case l1 :: l2 :: ls =>
      val l = l2.trim
      if (join.exists(l.startsWith))
        merge(result, (l1 ++ l) :: ls)
      else
        merge(result :+ l1, l2 :: ls)
    case l :: Nil => result :+ l
    case Nil => result
  }
}
