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


final class HtmlTableWriter extends TableWriter {

    override protected def writeStartTable {
        writeln(startTag("table"))
        writeln(startTag("thead"))
        writeln(startTag("tr"))
    }


    override protected def writeColumnHeader(name: String) {
        write(startTag("th"))
        write(name)
        write(endTag("th"))
        writeln
    }


    override protected def writeHeaderEnd {
        writeln(endTag("tr"))
        writeln(endTag("thead"))
        writeln(startTag("tbody"))
    }


    override protected def writeStartRow {
        writeln(startTag("tr"))
    }


    override protected def writeValue(value: String) {
        write(startTag("td"))
        write(value)
        write(startTag("td"))
        writeln
    }


    override protected def writeEndRow {
        writeln(endTag("tr"))
    }


    override protected def writeEndTable {
        writeln(endTag("tbody"))
        writeln(endTag("table"))
    }


    private def startTag(name: String): String = "<" + name + ">"


    private def endTag(name: String): String = "</" + name + ">"


    private def write(what: String) = print(what)


    private def writeln(what: String) = println(what)


    private def writeln() = println
}
