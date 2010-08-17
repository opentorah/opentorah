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

import scala.xml.{Node, XML, Utility}


final class TextDescriptor(val name: String, val isEdit: Boolean, path: String) extends Text {

    def getXml() = getXml(path)


    private def getXml(path: String) = {
        if (xml == null) {
            xml = Utility.trimProper(XML.loadFile(path))
        }

        xml
    }


    private var xml: Seq[Node] = null
}

