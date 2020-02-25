package org.digitaljudaica.xml

import java.io.{File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter, Writer}
import scala.xml.{Elem, Node}

object Print {

  private val width = 120

  private val prettyPrinter: scala.xml.PrettyPrinter = new scala.xml.PrettyPrinter(width, 2)

  private val join: Set[String] = Set(".", ",", ";", ":", "\"", ")")

  def format(elem: Elem): String = {
    val result: String = prettyPrinter.format(elem)

    // pretty-printer splits punctuation from the preceding elements; merge them back :)
    merge(List.empty, result.split("\n").toList).mkString("\n")
  }

  @scala.annotation.tailrec
  def merge(result: List[String], lines: List[String]): List[String] = lines match {
    case l1 :: l2 :: ls =>
      val l = l2.trim
      if (join.exists(l.startsWith))
        merge(result, (l1 ++ l) :: ls)
      else
        merge(result :+ l1, l2 :: ls)
    case l :: Nil => result :+ l
    case Nil => result
  }

  def print(xml: Node, outStream: OutputStream): Unit = print(xml, new OutputStreamWriter(outStream))
  def print(xml: Node, outFile: File): Unit = print(xml, new FileWriter(outFile))

  def print(xml: Node, writer: Writer): Unit = {
    val out = new PrintWriter(writer)
    val pretty = prettyPrinter.format(xml)
    // TODO when outputting XML, include <xml> header?
    out.println(pretty)
    out.close()
  }

  def spacedText(node: Node): String = node match {
    case elem: Elem => (elem.child map (_.text)).mkString(" ")
    case node: Node => node.text
  }
}
