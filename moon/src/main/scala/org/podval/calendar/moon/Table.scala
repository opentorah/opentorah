/*
 * Copyright 2011 Podval Group.
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

package org.podval.calendar.moon

import scala.xml.PrettyPrinter
import java.io.{PrintStream, File, FileOutputStream}


class Column[A](val heading: String, val subheading: String, val f: A => Any)


class Table[A](columns: List[Column[A]], rows: List[A]) {

    private def toHtml = {
        <html>
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
            </head>
            <body>
                <table border="1">
                    <colgroup>{
                                  for (c <- columns) yield <col/>
                              }</colgroup>
                    <thead>
                        <tr>{
                                for (c <- columns) yield <th>{c.heading}</th>
                            }</tr>
                        <tr>{
                                for (c <- columns) yield <th>{c.subheading}</th>
                            }</tr>
                    </thead>
                    <tbody>{
                            for (r <- rows) yield
                                <tr>{
                                        for (c <- columns) yield <td>{c.f(r)}</td>
                                    }</tr>
                        }</tbody>
                </table>
            </body>
        </html>
    }


    def writeHtml(out: PrintStream) {
        val what = new PrettyPrinter(80, 2).format(toHtml)
        out.print(what)
    }


    def writeHtml(file: File) {
        val out = new PrintStream(new FileOutputStream(file))
        writeHtml(out)
        out.close
    }


    def writeHtml(path: String) {
        writeHtml(new File(path))
    }
}
