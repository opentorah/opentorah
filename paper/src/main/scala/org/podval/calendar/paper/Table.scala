/*
 * Copyright 2011-2018 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.calendar.paper

import scala.xml.{Elem, PrettyPrinter}
import java.io.{File, FileOutputStream, PrintStream}


final case class Column[A](heading: String, subheading: String, f: A => Any)


final class Columns[A](suffix: String, columns: Column[A]*) {

  def tabulate(name: String, rows: Seq[A]): Table[A] = new Table(name + "-" + suffix, columns, rows)
}


final class Table[A](name: String, columns: Seq[Column[A]], rows: Seq[A]) {

  private def toHtml: Elem =
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
        <table border="1">
          <colgroup>
            {for (c <- columns) yield <col/>}
          </colgroup>
          <thead>
            <tr>{for (c <- columns) yield <th>{c.heading}</th>}</tr>
            <tr>{for (c <- columns) yield <th>{c.subheading}</th>}</tr>
          </thead>
          <tbody>
            {for (r <- rows) yield
            <tr>{for (c <- columns) yield <td>{c.f(r)}</td>}</tr>}
          </tbody>
        </table>
      </body>
    </html>


  private def toDocBook: Elem =
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


  def write(directory: File): Unit = {
    println("writing table " + name)
    writeHtml(open(directory, name, "html"))
    writeDocBook(open(directory, name, "xml"))
  }


  private[this] def open(directory: File, name: String, extension: String): PrintStream =
    new PrintStream(new FileOutputStream(new File(directory, name + "." + extension)))


  private[this] def writeHtml(out: PrintStream): Unit = {
    write(toHtml, out)
  }


  private[this] def writeDocBook(out: PrintStream): Unit = {
    out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    write(toDocBook, out)
  }


  private[this] def write(xml: Elem, out: PrintStream): Unit = {
    val what = new PrettyPrinter(80, 2).format(toDocBook)
    out.print(what)
    out.println()
  }
}
