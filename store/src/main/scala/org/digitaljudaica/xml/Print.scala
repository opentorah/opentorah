package org.digitaljudaica.xml

import java.io.{File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter, Writer}

import scala.xml.{Elem, Node, PrettyPrinter}

object Print {

  private val width = 120

  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(width, 2) {
    // TODO: it seems that there is a bug in PrettyPrinter, but with this override uncommented stack overflows...
    //    override protected def makeBox(ind: Int, s: String): Unit =
    //      if (cur + s.length <= width) { // fits in this line; LMD: changed > to <=...
    //        items ::= Box(ind, s)
    //        cur += s.length
    //      } else try cut(s, ind) foreach (items ::= _) // break it up
    //      catch { case _: BrokenException => makePara(ind, s) } // give up, para
  }

  // TODO PrettyPrinter breaks the line between e1 and e2 in <e1>...</e1><e2>...</e2>
  // and between e1 and text in: <e1>...<e1>text;
  // should I try fixing that?
  // or implement my own based on http://www.lihaoyi.com/post/CompactStreamingPrettyPrintingofHierarchicalData.html ?
  // or move to DOM and use org.apache.xml.serializer.dom3.LSSerializerImpl?
  // or move via DOM to ScalaTags (implementation "com.lihaoyi:scalatags_$scalaVersionMajor:$scalaTagsVersion")?
  // or use `spotless` with the Eclipse formatter?
  private val join: Set[String] = Set(".", ",", ";", ":", "\"", ")")

  def format(elem: Elem): String = {
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

    val result: String = prettyPrinter.format(elem)

    // pretty-printer splits punctuation from the preceding elements; merge them back :)
    merge(List.empty, result.split("\n").toList).mkString("\n")
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
