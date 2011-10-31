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

import java.io.{PrintStream, File, FileOutputStream}


object DayTables {

    def tables(name: String, data: Map[Int, Angle]) = {
        val days = new Column("days", "n", (days: Int) => days)
        val value = new Column("value", "v(n)", (days: Int) => data(days))
        val calculated = new Column("calculated", "v(1)*n", (days: Int) => data(1)*days)

        List(
            table(name, "original", days, value),
            table(name, "calculated", days, value, calculated)
        )
    }
    

    private def table(name: String, suffix: String, columns: Column[Int]*) =
        new Table[Int](name+"-"+suffix, columns.toList, DayTablesData.Order)


    def write[A](directory: File, tables: List[Table[A]]) {
        for (table <- tables) {
            table.writeHtml(open(directory, table.name, "html"))
            table.writeDocBook(open(directory, table.name, "xml"))
        }
    }


    private def open(directory: File, name: String, extension: String): PrintStream =
        new PrintStream(new FileOutputStream(new File(directory, name+"."+extension)))


    def main(args: Array[String]) {
        val directory = new File(args(0))
        directory.mkdir
        write(directory, tables("mml", DayTablesData.moonMeanLongitude))
    }
}
