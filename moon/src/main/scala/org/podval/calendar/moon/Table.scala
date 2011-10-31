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
import java.io.PrintStream


final class Column[A](val heading: String, val subheading: String, val f: A => Any)


final class Table[A](val name: String, columns: List[Column[A]], rows: List[A]) {

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


    private def toDocBook =
//        <section xmlns="http://docbook.org/ns/docbook" version="5.0">
//            <title>{name}</title>
            <informaltable xmlns="http://docbook.org/ns/docbook" version="5.0" frame="all" xml:id={name}>
                <tgroup cols={columns.length.toString}>
                    <thead>
                        <row>{
                            for (c <- columns) yield <entry>{c.heading}</entry>
                        }</row>
                        <row>{
                            for (c <- columns) yield <entry>{c.subheading}</entry>
                        }</row>
                    </thead>
                    <tbody>{
                        for (r <- rows) yield
                        <row>{
                            for (c <- columns) yield <entry>{c.f(r)}</entry>
                        }</row>
                    }</tbody>
                </tgroup>
            </informaltable>
//        </section>


    def writeHtml(out: PrintStream) {
        val what = new PrettyPrinter(80, 2).format(toHtml)
        out.print(what)
        out.println()
    }


    def writeDocBook(out: PrintStream) {
        out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
        val what = new PrettyPrinter(80, 2).format(toDocBook)
        out.print(what)
        out.println()
    }
}
