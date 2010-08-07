/*
 * Copyright 2010 dub.
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
 * under the License.
 */

package org.podval.judaica.viewer

import scala.xml.{XML, PrettyPrinter, Utility}


object Main {

    def main(args: Array[String]) {
//        val viewer = new tanach.Viewer(
//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/jerusalem/Genesis.xml"),
//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/toronto/Genesis.xml")
//        )

//        val xml = viewer.merge()
        val j = loadFile("/tmp/j.xml")
        val t = loadFile("/tmp/t.xml")

        val merged = Merger.merge(j, t, List("book", "chapter", "verse"))
        val trimmed = merged.flatMap(Utility.trimProper(_))
        val xml = Formatter.format(trimmed, "verse")

        println(new PrettyPrinter(80, 4).formatNodes(xml));
    }


    private def loadFile(path: String) = Utility.trimProper(XML.loadFile(path))
}
