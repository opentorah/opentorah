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


final class TextTableWriter extends TableWriter {

    protected override def writeStartTable = write("|")


    protected override def writeColumnHeader(name: String) {
        write(name)
        write("|")
    }


    protected override def writeHeaderEnd = writeln()


    protected override def writeStartRow = write("|")


    protected override def writeValue(value: String) {
        write(value)
        write("|")
    }


    protected override def writeEndRow = writeln()


    protected override def writeEndTable = {}


    private def write(what: Any) {
        print(what)
    }


    private def writeln() {
        println
    }
}
