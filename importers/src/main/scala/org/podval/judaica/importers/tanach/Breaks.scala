/*
 *  Copyright 2011 dub.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.judaica.importers.tanach

import scala.xml.{XML, Node, Utility}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Breaks {

    def get(name: String)/*: Map[String, Map[String, Seq[Node]]]*/ = {
        val result = mutable.Map.empty[String, mutable.Map[String, ListBuffer[Node]]]

        val xml = Utility.trimProper(XML.load(getClass.getResourceAsStream(name + ".xml")))

        for (child <- xml(0).child) {
            val chapter = (child \ "@chapter").text
            val verse = (child \ "@verse").text

            if (!result.contains(chapter)) {
                result(chapter) = scala.collection.mutable.Map.empty[String, ListBuffer[Node]]
            }
            val verses = result(chapter)

            if (!verses.contains(verse)) {
                verses(verse) = ListBuffer.empty[Node]
            }
            val breaks = verses(verse)

            breaks += child
        }

        /* TODO Turn immutable Map.empty[String, Map[String, ListBuffer[Node]]] ++ */ result
    }


    def main(args: Array[String]) {
        Console.println(get("Genesis"))
    }
}
