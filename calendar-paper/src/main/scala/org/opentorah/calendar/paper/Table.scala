package org.opentorah.calendar.paper

import java.io.{File, FileOutputStream, PrintStream}
import scala.xml.{Elem, PrettyPrinter}

final case class Column[A](heading: String, subheading: String, f: A => Any)

final class Table[A](columns: Column[A]*) {

  def write(directory: File, name: String, rows: Seq[A]): Unit = {
    val xml: Elem =
      <informaltable xmlns="http://docbook.org/ns/docbook" version="5.0" frame="all" xml:id={name}>
        <tgroup cols={columns.length.toString}>
          <thead>
            <row>{for (c <- columns) yield <entry>{c.heading}</entry>}</row>
            <row>{for (c <- columns) yield <entry>{c.subheading}</entry>}</row>
          </thead>
          <tbody>
            {for (r <- rows) yield
            <row>{for (c <- columns) yield <entry>{c.f(r)}</entry>}</row>}
          </tbody>
        </tgroup>
      </informaltable>

    val out: PrintStream = new PrintStream(new FileOutputStream(new File(directory, name + ".xml")))
    out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    // TODO use my pretty-printer and remove scala.xml imports
    out.print(new PrettyPrinter(80, 2).format(xml))
    out.println()
  }
}
