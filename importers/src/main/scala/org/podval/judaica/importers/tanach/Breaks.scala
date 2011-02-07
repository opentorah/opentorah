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

import scala.xml.{XML, Source, Elem}

import java.io.File


object Breaks {

    def get(file: File) = {
        val xml: Elem = XML.load(Source.fromFile(file))
        for (child <- xml.child) {
          Console.out.println(child.attribute("chapter"))
        }
    }


    def main(args: Array[String]) {
        get(new File("/home/dub/projects-judaica/metadata/meta", "Genesis.xml"))
    }
}
