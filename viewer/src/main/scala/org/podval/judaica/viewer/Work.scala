/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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

package org.podval.judaica.viewer

import org.podval.judaica.common.Xml

import java.io.File


final class Work(names: Names, directory: String) {

    def hasName(name: String): Boolean = names.hasName(name)


    override def toString: String = "Work (" + directory + ") " + names
}



object Work {

    def apply(file: File): Work = {
        val xml = Xml.loadFile(file, "work")

        new Work(
            Names(xml),
            Xml.getAttribute("directory")(xml)
        )
    }
}