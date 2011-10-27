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


import scala.collection.mutable.ListBuffer

trait TableWriter {

    private var columnNames = ListBuffer[String]()


    private var started = false


    private var done = false


    private var rowsStarted = false


    def numberOfColumns = columnNames.length


    private var columnNumber = 0


    final def addColumn(name: String) {
        checkNotDone
        checkRowsNotStarted

        startTable

        writeColumnHeader(name)

        columnNames += name
    }


    private def startTable {
        if (!started) {
            checkNotDone

            writeStartTable

            started = true
        }
    }


    def startRow {
        checkNotDone

        endRow
        writeStartRow
    }


    private def endRow {
        checkNotDone

        if (rowsStarted) {
            if (columnNumber != numberOfColumns)
                throw new IllegalStateException("Not at end of row")

            writeEndRow

        } else {
            if (numberOfColumns == 0)
                throw new IllegalArgumentException("Empty table")

            writeHeaderEnd

            rowsStarted = true
        }

        columnNumber = 0
    }


    def value(value: Any) {
        checkNotDone
        checkRowsStarted

        if (columnNumber >= numberOfColumns)
            throw new IllegalStateException("Row is already done")

        writeValue(value.toString)

        columnNumber += 1
    }


    final def endTable {
        checkNotDone
        checkRowsStarted

        endRow
        writeEndTable

        done = true
    }


    private def checkNotDone {
        if (done)
            throw new IllegalStateException("Table is already done")
    }


    private def checkRowsNotStarted {
        if (rowsStarted)
            throw new IllegalStateException("Rows already started")
    }


    private def checkRowsStarted {
        if (!rowsStarted)
            throw new IllegalStateException("Rows did not start")
    }


    protected def writeStartTable


    protected def writeColumnHeader(name: String)


    protected def writeHeaderEnd


    protected def writeStartRow


    protected def writeValue(value: String)


    protected def writeEndRow


    protected def writeEndTable
}
