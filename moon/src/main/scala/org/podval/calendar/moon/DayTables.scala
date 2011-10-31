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


final class TableWithSuffix[A](val suffix: String, val table: Table[A])


object DayTables {

    def tables(data: Map[Int, Angle]) = {
        val days = new Column("days", "n", (days: Int) => days)
        val value = new Column("value", "v(n)", (days: Int) => data(days))
        val calculated = new Column("calculated", "v(1)*n", (days: Int) => data(1)*days)

        List(
            table("original", days, value),
            table("calculated", days, value, calculated)
        )
    }
    

    private def table(suffix: String, columns: Column[Int]*) =
        new TableWithSuffix[Int](suffix, new Table[Int](columns.toList, DayTablesData.Order))


    def write[A](path: String, name: String, tables: List[TableWithSuffix[A]]) {
        for (t <- tables) {
            t.table.writeHtml(path + name + "-" + t.suffix + ".html")
        }
    }


    def main(args: Array[String]) {
        write("/tmp/xxx/", "mml", tables(DayTablesData.moonMeanLongitude))
    }
}
