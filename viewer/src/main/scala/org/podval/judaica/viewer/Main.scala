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

import scala.xml.PrettyPrinter

import org.podval.judaica.viewer.tanach.TanachTextFormat


object Main {

    def main(args: Array[String]) = {
        new PrettyPrinter(100,2).formatNodes(doIt("/judaica/"))
    }


    def doIt(postUrl: String) = {
//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/jerusalem/Genesis.xml"),
//            new Text("/var/www/sites/hg.judaica/projects/texts/Tanach/toronto/Genesis.xml")

        new Viewer(
            new TanachTextFormat(),
            "chapter",
            List(
                new TextDescriptor("jerusalem", false, "/tmp/j.xml"),
                new TextDescriptor("toronto", true, "/tmp/t.xml")
            )
        ).format(postUrl)
 
// @todo blog about the spaces in Scala's XML literals if curly braces are not flush with the tags!
//        val trimmed = merged.flatMap(Utility.trimProper(_))
//        
// Also, about utility of the Seq[Node] writer...
    }
}
